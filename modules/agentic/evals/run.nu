#!/usr/bin/env nu

# Parse YAML frontmatter from a markdown string (content between first two --- delimiters).
def parse-frontmatter []: string -> record {
    let lines = ($in | lines)
    let delims = ($lines | enumerate | where {|e| $e.item == '---'} | get index)
    if ($delims | length) < 2 { return {} }
    $lines
    | skip ($delims.0 + 1)
    | take (($delims.1) - ($delims.0 + 1))
    | str join "\n"
    | from yaml
}

# Extract the prompt body (everything after the second --- delimiter, leading blank lines stripped).
def extract-body []: string -> string {
    let lines = ($in | lines)
    let delims = ($lines | enumerate | where {|e| $e.item == '---'} | get index)
    if ($delims | length) < 2 { return "" }
    $lines | skip (($delims.1) + 1) | str join "\n" | str trim --left
}

# Test whether a scenario id matches a shell-glob filter pattern.
def matches-filter [filter: string]: string -> bool {
    if $filter == '*' { return true }
    let id = $in
    let pattern = ($filter | str replace --all '*' '.*' | str replace --all '?' '.')
    $id =~ $"^($pattern)$"
}

# Flatten a pi session JSONL into a human-readable transcript.
# Mirrors extract-transcript.sh: toolResult text capped at 300 chars, toolCall args at 200.
def extract-transcript [jsonl_path: string]: nothing -> string {
    open --raw $jsonl_path
    | lines
    | where {|l| ($l | str trim) != ''}
    | each {|line| $line | from json}
    | where type == "message"
    | each {|entry|
        let msg = $entry.message
        let content = ($msg | get -o content | default [])
        $content | each {|block|
            if $msg.role == "user" and $block.type == "text" {
                $"[user]\n($block.text)\n"
            } else if $msg.role == "assistant" and $block.type == "text" {
                $"[assistant]\n($block.text)\n"
            } else if $msg.role == "assistant" and $block.type == "toolCall" {
                let raw_args = ($block | get -o arguments | default {})
                let args_str = if ($raw_args | describe | str starts-with "string") {
                    $raw_args
                } else {
                    $raw_args | to json
                }
                let truncated = ($args_str | str substring 0..199)
                $"[toolCall] ($block.name)\(($truncated)\)\n"
            } else if $msg.role == "toolResult" and $block.type == "text" {
                let truncated = ($block.text | str substring 0..299)
                $"[toolResult] ($truncated)\n"
            } else {
                ""
            }
        } | str join
    }
    | str join
}

# Render the pass/fail report table to stdout.
def render-report [rows: list<record>]: nothing -> nothing {
    print $"($"STATUS" | fill -a l -w 7)  ($"ID" | fill -a l -w 32)  TITLE"
    print ('' | fill -c '-' -w 72)
    for row in $rows {
        let sid = ($row | get -o id | default '' | into string)
        let stitle = ($row | get -o title | default '' | into string)
        match $row.status {
            "PASS" => {
                print $"PASS     ($sid | fill -a l -w 32)  ($stitle)"
            }
            "FAIL" => {
                print $"FAIL     ($sid | fill -a l -w 32)  ($stitle)"
                let vf = ($row | get -o verdict_file | default '' | into string)
                if $vf != '' and ($vf | path exists) {
                    let verdict = (open $vf)
                    for a in ($verdict.assertions | where status == "FAIL") {
                        print $"           FAIL [($a.id)]: ($a.reason)"
                    }
                }
            }
            "ERROR" => {
                let errmsg = ($row | get -o error | default 'unknown' | into string)
                print $"ERROR    ($sid | fill -a l -w 32)  [($errmsg)]"
            }
            _ => {
                print $"($row.status | fill -a l -w 7)  ($sid | fill -a l -w 32)  ($stitle)"
            }
        }
    }
}

# Eval harness for agentic skills.
# Runs each matched scenario through pi, then judges the transcript against per-scenario assertions.
#
# Exit codes:
#   0  all matched scenarios passed
#   1  one or more scenarios failed
#   2  one or more scenarios errored (harness fault, not a skill failure)
def main [
    --skill: string = ""          # Skill directory to test
    --eval-dir: string = ""       # Directory containing scenarios/ and fixtures/
    --filter: string = "*"        # Glob filter on scenario id (default: all)
    --judge-model: string = "anthropic/claude-sonnet-4-20250514"  # Model for LLM judge
    --subject-model: string = ""  # Model for skill under test (default: same as judge-model)
    --out: string = ""            # Output directory (default: .eval-runs/<utc-ts>)
    --keep-transcripts            # Retain raw JSONL session files after run
] {
    if $skill == "" {
        print -e "error: --skill is required"
        exit 2
    }
    if $eval_dir == "" {
        print -e "error: --eval-dir is required"
        exit 2
    }
    if not ($skill | path exists) {
        print -e $"error: --skill path not found: ($skill)"
        exit 2
    }
    if not ($eval_dir | path exists) {
        print -e $"error: --eval-dir path not found: ($eval_dir)"
        exit 2
    }

    let skill_abs = ($skill | path expand)
    let eval_dir_abs = ($eval_dir | path expand)
    let effective_subject_model = if $subject_model == "" { $judge_model } else { $subject_model }
    let lib_dir = ($env.CURRENT_FILE | path dirname | path join "lib")

    let run_out = if $out == "" {
        let ts = (date now | format date "%Y%m%dT%H%M%SZ")
        $".eval-runs/($ts)"
    } else {
        $out
    }
    mkdir $run_out
    let run_out_abs = ($run_out | path expand)

    let scenarios_dir = ($eval_dir_abs | path join "scenarios")
    if not ($scenarios_dir | path exists) {
        print -e $"error: scenarios directory not found: ($scenarios_dir)"
        exit 2
    }

    # Discover and filter scenarios by id
    mut matched = []
    for f in (glob $"($scenarios_dir)/*.md" | sort) {
        let raw = (open --raw $f)
        let fm = ($raw | parse-frontmatter)
        let sid = ($fm | get -o id | default '' | into string)
        if $sid == '' {
            print -e $"warning: could not read .id from ($f) — skipping"
            continue
        }
        if ($sid | matches-filter $filter) {
            $matched = ($matched | append $f)
        }
    }

    if ($matched | is-empty) {
        print $"0 scenarios run \(no matches for filter: ($filter)\)"
        exit 0
    }

    print $"Running ($matched | length) scenario\(s\) — artifacts: ($run_out_abs)\n"

    mut pass_count = 0
    mut fail_count = 0
    mut error_count = 0
    mut manifest = []

    for scenario_file in $matched {
        let raw = (open --raw $scenario_file)
        let fm = ($raw | parse-frontmatter)

        let sid = ($fm | get -o id | default '' | into string)
        if $sid == '' {
            print -e $"ERROR: could not read .id from ($scenario_file)"
            $error_count = $error_count + 1
            $manifest = ($manifest | append {status: "ERROR", id: "unknown", error: "could not read .id"})
            continue
        }

        let stitle = ($fm | get -o title | default $sid | into string)

        let fixture_rel = ($fm | get -o fixture | default '' | into string)
        if $fixture_rel == '' {
            print -e $"ERROR [($sid)]: could not read .fixture"
            $error_count = $error_count + 1
            $manifest = ($manifest | append {status: "ERROR", id: $sid, error: "could not read .fixture"})
            continue
        }

        let expect_trigger = ($fm | get -o expect_trigger | default true | into string)
        let target_lens_raw = ($fm | get -o target_lens | default null)
        let target_lens = if $target_lens_raw == null { "null" } else { $target_lens_raw | into string }
        let category = ($fm | get -o category | default "pressure" | into string)
        let assertions_list = ($fm | get -o assertions | default [])

        let prompt = ($raw | extract-body)

        let scen_out = ($run_out_abs | path join $sid)
        mkdir $"($scen_out)/sessions"

        let fixture_path = ($eval_dir_abs | path join $fixture_rel)
        if not ($fixture_path | path exists) {
            print -e $"ERROR [($sid)]: fixture not found: ($fixture_path)"
            $error_count = $error_count + 1
            $manifest = ($manifest | append {status: "ERROR", id: $sid, error: "fixture not found"})
            continue
        }

        # Copy fixture to a per-scenario working dir so subject edits do not
        # mutate the source fixture and pollute subsequent runs.
        let fixture_work = ($scen_out | path join "fixture")
        mkdir $fixture_work
        ^cp -r $"($fixture_path)/." $fixture_work

        print $"running: ($sid)"

        # Run subject pi inside the per-scenario fixture copy
        let subject_result = (do { cd $fixture_work; ^pi --no-skills --skill $skill_abs --session-dir $"($scen_out)/sessions" --model $effective_subject_model -p $prompt } | complete)
        let subject_rc = $subject_result.exit_code
        $subject_result.stdout | save --force $"($scen_out)/subject.stdout"

        # Find the JSONL pi wrote (searches one and two levels deep)
        let jsonl_files = (glob $"($scen_out)/sessions/**/*.jsonl" | sort)
        if ($jsonl_files | is-empty) {
            print -e $"ERROR [($sid)]: no JSONL in session dir \(pi exit=($subject_rc)\)"
            print -e (open --raw $"($scen_out)/subject.stdout")
            $error_count = $error_count + 1
            $manifest = ($manifest | append {status: "ERROR", id: $sid, error: "no session JSONL"})
            continue
        }
        let latest_jsonl = ($jsonl_files | last)
        cp $latest_jsonl $"($scen_out)/subject.jsonl"

        # Flatten transcript to human-readable text
        let transcript = (extract-transcript $"($scen_out)/subject.jsonl")
        $transcript | save --force $"($scen_out)/subject.txt"

        # Format assertions as numbered list for the judge prompt
        let assertions_text = (
            $assertions_list
            | enumerate
            | each {|e|
                let a = $e.item
                let text = ($a.text | into string | str trim --right)
                $"($e.index + 1). [($a.id)] ($text)"
            }
            | str join "\n"
        )

        # Render judge prompt via literal string substitution (safe for backslashes and special chars)
        let judge_prompt = (
            open --raw $"($lib_dir)/judge-prompt.md"
            | str replace --all '{{id}}' $sid
            | str replace --all '{{title}}' $stitle
            | str replace --all '{{category}}' $category
            | str replace --all '{{expect_trigger}}' $expect_trigger
            | str replace --all '{{target_lens}}' $target_lens
            | str replace --all '{{assertions}}' $assertions_text
            | str replace --all '{{transcript}}' ($transcript | str trim --right)
        )
        $judge_prompt | save --force $"($scen_out)/judge-prompt.txt"

        # Run judge pi (no session, no skill, just text output)
        let judge_result = (^pi --no-skills --no-session --model $judge_model -p $judge_prompt | complete)
        if $judge_result.exit_code != 0 {
            print -e $"ERROR [($sid)]: judge pi failed"
            $error_count = $error_count + 1
            $manifest = ($manifest | append {status: "ERROR", id: $sid, error: "judge pi failed"})
            continue
        }
        let judge_stdout = $judge_result.stdout

        # Extract fenced JSON block from judge output
        let json_blocks = ($judge_stdout | parse --regex '(?s)```json\n(?P<json>.*?)\n```')
        if ($json_blocks | is-empty) {
            print -e $"ERROR [($sid)]: judge produced no JSON block"
            $judge_stdout | save --force $"($scen_out)/judge-raw.txt"
            $error_count = $error_count + 1
            $manifest = ($manifest | append {status: "ERROR", id: $sid, error: "no JSON block from judge"})
            continue
        }
        let verdict_json_str = ($json_blocks | get json | first)

        # Validate and save verdict
        let verdict = try { $verdict_json_str | from json } catch { null }
        if $verdict == null {
            print -e $"ERROR [($sid)]: invalid JSON from judge"
            $verdict_json_str | save --force $"($scen_out)/verdict-raw.txt"
            $error_count = $error_count + 1
            $manifest = ($manifest | append {status: "ERROR", id: $sid, error: "invalid verdict JSON"})
            continue
        }
        $verdict | to json | save --force $"($scen_out)/verdict.json"

        let overall = ($verdict | get -o overall | default "FAIL" | into string)

        if $overall == "PASS" {
            $pass_count = $pass_count + 1
            $manifest = ($manifest | append {status: "PASS", id: $sid, title: $stitle})
        } else {
            $fail_count = $fail_count + 1
            $manifest = ($manifest | append {status: "FAIL", id: $sid, title: $stitle, verdict_file: $"($scen_out)/verdict.json"})
        }

        if not $keep_transcripts {
            rm --force $"($scen_out)/subject.jsonl"
        }
    }

    # Write manifest as JSONL (one JSON object per line)
    $manifest
    | each {|row| $row | to json | lines | str trim | str join ""}
    | str join "\n"
    | save --force ($run_out_abs | path join "manifest.jsonl")

    print ""
    render-report $manifest
    print $"\nResults: ($pass_count) passed, ($fail_count) failed, ($error_count) errored  \(artifacts: ($run_out_abs)\)"

    if $error_count > 0 { exit 2 }
    if $fail_count > 0 { exit 1 }
}
