#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LIB_DIR="$SCRIPT_DIR/lib"

SKILL=""
EVAL_DIR=""
FILTER="*"
JUDGE_MODEL="anthropic/claude-sonnet-4-20250514"
SUBJECT_MODEL="anthropic/claude-sonnet-4-20250514"
OUT_DIR=""
KEEP_TRANSCRIPTS=false

usage() {
  cat <<'EOF'
Usage: run.sh --skill <path> --eval-dir <path> [options]

Required:
  --skill <path>                Skill directory to test
  --eval-dir <path>             Directory containing scenarios/ and fixtures/

Options:
  --filter <glob>               Run only scenarios whose id matches this glob (default: *)
  --judge-model <provider/id>   Model for the LLM judge (default: anthropic/claude-sonnet-4-20250514)
  --subject-model <provider/id> Model for the skill under test (default: same as judge)
  --out <dir>                   Output directory (default: .eval-runs/<utc-ts>/)
  --keep-transcripts            Keep raw JSONL session files after run
  -h, --help                    Show this help

Exit codes:
  0  all matched scenarios passed
  1  one or more scenarios failed
  2  one or more scenarios errored (harness fault, not a skill failure)
EOF
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --skill)             SKILL="$2";           shift 2 ;;
    --eval-dir)          EVAL_DIR="$2";        shift 2 ;;
    --filter)            FILTER="$2";          shift 2 ;;
    --judge-model)       JUDGE_MODEL="$2";     shift 2 ;;
    --subject-model)     SUBJECT_MODEL="$2";   shift 2 ;;
    --out)               OUT_DIR="$2";         shift 2 ;;
    --keep-transcripts)  KEEP_TRANSCRIPTS=true; shift ;;
    -h|--help)           usage; exit 0 ;;
    *) printf 'error: unknown option: %s\n' "$1" >&2; usage >&2; exit 2 ;;
  esac
done

[[ -z "$SKILL" ]]    && { printf 'error: --skill is required\n' >&2;    usage >&2; exit 2; }
[[ -z "$EVAL_DIR" ]] && { printf 'error: --eval-dir is required\n' >&2; usage >&2; exit 2; }

if ! SKILL="$(cd "$SKILL" 2>/dev/null && pwd)"; then
  printf 'error: --skill path not found: %s\n' "$SKILL" >&2; exit 2
fi
if ! EVAL_DIR="$(cd "$EVAL_DIR" 2>/dev/null && pwd)"; then
  printf 'error: --eval-dir path not found: %s\n' "$EVAL_DIR" >&2; exit 2
fi

if [[ -z "$OUT_DIR" ]]; then
  if ! TS="$(date -u '+%Y%m%dT%H%M%SZ')"; then
    TS="run-$(date +%s)"
  fi
  OUT_DIR=".eval-runs/$TS"
fi
mkdir -p "$OUT_DIR"
OUT_DIR="$(cd "$OUT_DIR" && pwd)"

SCENARIOS_DIR="$EVAL_DIR/scenarios"
[[ -d "$SCENARIOS_DIR" ]] || {
  printf 'error: scenarios directory not found: %s\n' "$SCENARIOS_DIR" >&2; exit 2
}

# Discover and filter scenarios by id
MATCHED=()
while IFS= read -r f; do
  sid=""
  if ! sid="$(md 'frontmatter | .id' "$f" 2>/dev/null)"; then
    printf 'warning: could not read .id from %s — skipping\n' "$f" >&2
    continue
  fi
  # shellcheck disable=SC2053 — intentional glob match on $FILTER
  if [[ "$sid" == $FILTER ]]; then
    MATCHED+=("$f")
  fi
done < <(find "$SCENARIOS_DIR" -maxdepth 1 -name "*.md" | sort)

if [[ ${#MATCHED[@]} -eq 0 ]]; then
  printf '0 scenarios run (no matches for filter: %s)\n' "$FILTER"
  exit 0
fi

printf 'Running %d scenario(s) — artifacts: %s\n\n' "${#MATCHED[@]}" "$OUT_DIR"

PASS_COUNT=0
FAIL_COUNT=0
ERROR_COUNT=0
MANIFEST_FILE="$OUT_DIR/manifest.jsonl"
> "$MANIFEST_FILE"

# Render judge prompt into $out_file by substituting {{placeholders}} in the template.
# Uses python3 str.replace to handle backslashes and special characters safely.
render_judge_prompt() {
  local out_file="$1" id="$2" title="$3" category="$4" expect_trigger="$5"
  local target_lens="$6" assertions_file="$7" transcript_file="$8"

  python3 - \
    "$LIB_DIR/judge-prompt.md" \
    "$id" "$title" "$category" "$expect_trigger" "$target_lens" \
    "$assertions_file" "$transcript_file" \
    "$out_file" <<'PYEOF'
import sys
(template_path, id_, title, category, expect_trigger,
 target_lens, assertions_file, transcript_file, out_file) = sys.argv[1:]
t = open(template_path).read()
subs = {
    '{{id}}':             id_,
    '{{title}}':          title,
    '{{category}}':       category,
    '{{expect_trigger}}': expect_trigger,
    '{{target_lens}}':    target_lens,
    '{{assertions}}':     open(assertions_file).read().rstrip('\n'),
    '{{transcript}}':     open(transcript_file).read().rstrip('\n'),
}
for k, v in subs.items():
    t = t.replace(k, v)
with open(out_file, 'w') as fh:
    fh.write(t)
PYEOF
}

for scenario_file in "${MATCHED[@]}"; do
  # --- Parse frontmatter ---
  sid=""
  if ! sid="$(md 'frontmatter | .id' "$scenario_file" 2>/dev/null)"; then
    printf 'ERROR: could not read .id from %s\n' "$scenario_file" >&2
    ERROR_COUNT=$((ERROR_COUNT + 1))
    printf '{"status":"ERROR","id":"unknown","error":"could not read .id"}\n' >> "$MANIFEST_FILE"
    continue
  fi

  stitle="$sid"
  if ! stitle="$(md 'frontmatter | .title' "$scenario_file" 2>/dev/null)"; then
    stitle="$sid"
  fi

  fixture_rel=""
  if ! fixture_rel="$(md 'frontmatter | .fixture' "$scenario_file" 2>/dev/null)"; then
    printf 'ERROR [%s]: could not read .fixture\n' "$sid" >&2
    ERROR_COUNT=$((ERROR_COUNT + 1))
    printf '{"status":"ERROR","id":"%s","error":"could not read .fixture"}\n' "$sid" >> "$MANIFEST_FILE"
    continue
  fi

  expect_trigger="true"
  if ! expect_trigger="$(md 'frontmatter | .expect_trigger' "$scenario_file" 2>/dev/null)"; then
    expect_trigger="true"
  fi

  target_lens="null"
  if ! target_lens="$(md 'frontmatter | .target_lens' "$scenario_file" 2>/dev/null)"; then
    target_lens="null"
  fi

  category="pressure"
  if ! category="$(md 'frontmatter | .category' "$scenario_file" 2>/dev/null)"; then
    category="pressure"
  fi

  assertions_json=""
  if ! assertions_json="$(python3 - "$scenario_file" <<'PYEOF'
import sys, json, re
try:
    import yaml
except ImportError:
    import tomllib as _t; yaml = None

content = open(sys.argv[1]).read()
m = re.match(r'^---\n(.*?)\n---', content, re.DOTALL)
if not m:
    sys.exit(1)
if yaml:
    fm = yaml.safe_load(m.group(1))
else:
    import tomllib
    fm = tomllib.loads(m.group(1))
print(json.dumps(fm.get('assertions', [])))
PYEOF
  )"; then
    printf 'ERROR [%s]: could not read .assertions\n' "$sid" >&2
    ERROR_COUNT=$((ERROR_COUNT + 1))
    printf '{"status":"ERROR","id":"%s","error":"could not read .assertions"}\n' "$sid" >> "$MANIFEST_FILE"
    continue
  fi

  # --- Extract body (everything after the second ---) ---
  prompt=""
  if ! prompt="$(awk '/^---$/{n++;next} n>=2{print}' "$scenario_file")"; then
    printf 'ERROR [%s]: could not extract prompt body\n' "$sid" >&2
    ERROR_COUNT=$((ERROR_COUNT + 1))
    printf '{"status":"ERROR","id":"%s","error":"could not extract body"}\n' "$sid" >> "$MANIFEST_FILE"
    continue
  fi
  # Strip leading blank lines (guard the pipeline per pipefail memory)
  if ! prompt="$(printf '%s\n' "$prompt" | sed '/./,$!d')"; then
    printf 'ERROR [%s]: could not strip blank lines from prompt\n' "$sid" >&2
    ERROR_COUNT=$((ERROR_COUNT + 1))
    printf '{"status":"ERROR","id":"%s","error":"prompt processing failed"}\n' "$sid" >> "$MANIFEST_FILE"
    continue
  fi

  scen_out="$OUT_DIR/$sid"
  mkdir -p "$scen_out/sessions"

  fixture_path="$EVAL_DIR/$fixture_rel"
  if [[ ! -d "$fixture_path" ]]; then
    printf 'ERROR [%s]: fixture not found: %s\n' "$sid" "$fixture_path" >&2
    ERROR_COUNT=$((ERROR_COUNT + 1))
    printf '{"status":"ERROR","id":"%s","error":"fixture not found"}\n' "$sid" >> "$MANIFEST_FILE"
    continue
  fi

  printf 'running: %s\n' "$sid"

  # --- Run subject pi inside the fixture directory ---
  subject_rc=0
  (
    cd "$fixture_path"
    exec pi --no-skills --skill "$SKILL" \
      --session-dir "$scen_out/sessions" \
      --model "$SUBJECT_MODEL" \
      -p "$prompt"
  ) > "$scen_out/subject.stdout" 2>&1 || subject_rc=$?

  # --- Locate the JSONL pi wrote ---
  latest_jsonl=""
  for f in "$scen_out/sessions"/*.jsonl; do
    [[ -f "$f" ]] || continue
    if [[ -z "$latest_jsonl" || "$f" -nt "$latest_jsonl" ]]; then
      latest_jsonl="$f"
    fi
  done
  # Also search one level deeper in case pi creates a slug subdirectory
  if [[ -z "$latest_jsonl" ]]; then
    for f in "$scen_out/sessions"/*/*.jsonl; do
      [[ -f "$f" ]] || continue
      if [[ -z "$latest_jsonl" || "$f" -nt "$latest_jsonl" ]]; then
        latest_jsonl="$f"
      fi
    done
  fi
  if [[ -z "$latest_jsonl" ]]; then
    printf 'ERROR [%s]: no JSONL in session dir (pi exit=%d)\n' "$sid" "$subject_rc" >&2
    cat "$scen_out/subject.stdout" >&2
    ERROR_COUNT=$((ERROR_COUNT + 1))
    printf '{"status":"ERROR","id":"%s","error":"no session JSONL"}\n' "$sid" >> "$MANIFEST_FILE"
    continue
  fi
  cp "$latest_jsonl" "$scen_out/subject.jsonl"

  # --- Flatten transcript ---
  if ! "$LIB_DIR/extract-transcript.sh" "$scen_out/subject.jsonl" > "$scen_out/subject.txt"; then
    printf 'ERROR [%s]: transcript extraction failed\n' "$sid" >&2
    ERROR_COUNT=$((ERROR_COUNT + 1))
    printf '{"status":"ERROR","id":"%s","error":"transcript extraction failed"}\n' "$sid" >> "$MANIFEST_FILE"
    continue
  fi

  # --- Build assertions text for judge ---
  assertions_text=""
  if ! assertions_text="$(printf '%s' "$assertions_json" \
        | jq -r 'to_entries[] | "\(.key + 1). [\(.value.id)] \(.value.text | rtrimstr("\n"))"')"; then
    printf 'ERROR [%s]: could not format assertions\n' "$sid" >&2
    ERROR_COUNT=$((ERROR_COUNT + 1))
    printf '{"status":"ERROR","id":"%s","error":"assertions format failed"}\n' "$sid" >> "$MANIFEST_FILE"
    continue
  fi

  assertions_tmpfile="$(mktemp)"
  transcript_tmpfile="$(mktemp)"
  printf '%s' "$assertions_text" > "$assertions_tmpfile"
  cp "$scen_out/subject.txt" "$transcript_tmpfile"

  judge_prompt_file="$scen_out/judge-prompt.txt"
  if ! render_judge_prompt \
        "$judge_prompt_file" "$sid" "$stitle" "$category" \
        "$expect_trigger" "$target_lens" \
        "$assertions_tmpfile" "$transcript_tmpfile"; then
    rm -f "$assertions_tmpfile" "$transcript_tmpfile"
    printf 'ERROR [%s]: judge prompt render failed\n' "$sid" >&2
    ERROR_COUNT=$((ERROR_COUNT + 1))
    printf '{"status":"ERROR","id":"%s","error":"render_judge_prompt failed"}\n' "$sid" >> "$MANIFEST_FILE"
    continue
  fi
  rm -f "$assertions_tmpfile" "$transcript_tmpfile"

  # --- Run judge ---
  judge_prompt_contents=""
  if ! judge_prompt_contents="$(cat "$judge_prompt_file")"; then
    printf 'ERROR [%s]: could not read judge prompt file\n' "$sid" >&2
    ERROR_COUNT=$((ERROR_COUNT + 1))
    printf '{"status":"ERROR","id":"%s","error":"could not read judge prompt"}\n' "$sid" >> "$MANIFEST_FILE"
    continue
  fi

  judge_stdout=""
  if ! judge_stdout="$(pi --no-skills --no-session --model "$JUDGE_MODEL" -p "$judge_prompt_contents" 2>/dev/null)"; then
    printf 'ERROR [%s]: judge pi failed\n' "$sid" >&2
    ERROR_COUNT=$((ERROR_COUNT + 1))
    printf '{"status":"ERROR","id":"%s","error":"judge pi failed"}\n' "$sid" >> "$MANIFEST_FILE"
    continue
  fi

  # --- Extract fenced JSON block from judge output ---
  verdict_json=""
  if ! verdict_json="$(printf '%s\n' "$judge_stdout" \
        | awk '/^```json/{f=1;next} /^```/{if(f)exit} f')"; then
    printf 'ERROR [%s]: awk extraction failed\n' "$sid" >&2
    printf '%s\n' "$judge_stdout" > "$scen_out/judge-raw.txt"
    ERROR_COUNT=$((ERROR_COUNT + 1))
    printf '{"status":"ERROR","id":"%s","error":"awk extraction failed"}\n' "$sid" >> "$MANIFEST_FILE"
    continue
  fi
  if [[ -z "$verdict_json" ]]; then
    printf 'ERROR [%s]: judge produced no JSON block\n' "$sid" >&2
    printf '%s\n' "$judge_stdout" > "$scen_out/judge-raw.txt"
    ERROR_COUNT=$((ERROR_COUNT + 1))
    printf '{"status":"ERROR","id":"%s","error":"no JSON block from judge"}\n' "$sid" >> "$MANIFEST_FILE"
    continue
  fi

  # Validate and save verdict
  if ! printf '%s\n' "$verdict_json" | jq . > "$scen_out/verdict.json" 2>/dev/null; then
    printf 'ERROR [%s]: invalid JSON from judge\n' "$sid" >&2
    printf '%s\n' "$verdict_json" > "$scen_out/verdict-raw.txt"
    ERROR_COUNT=$((ERROR_COUNT + 1))
    printf '{"status":"ERROR","id":"%s","error":"invalid verdict JSON"}\n' "$sid" >> "$MANIFEST_FILE"
    continue
  fi

  overall="FAIL"
  if ! overall="$(jq -r '.overall' "$scen_out/verdict.json" 2>/dev/null)"; then
    overall="FAIL"
  fi

  if [[ "$overall" == "PASS" ]]; then
    PASS_COUNT=$((PASS_COUNT + 1))
    title_json=""
    if ! title_json="$(printf '%s' "$stitle" | jq -Rs .)"; then title_json='"?"'; fi
    printf '{"status":"PASS","id":"%s","title":%s}\n' "$sid" "$title_json" >> "$MANIFEST_FILE"
  else
    FAIL_COUNT=$((FAIL_COUNT + 1))
    title_json=""
    if ! title_json="$(printf '%s' "$stitle" | jq -Rs .)"; then title_json='"?"'; fi
    printf '{"status":"FAIL","id":"%s","title":%s,"verdict_file":"%s"}\n' \
      "$sid" "$title_json" "$scen_out/verdict.json" >> "$MANIFEST_FILE"
  fi

  [[ "$KEEP_TRANSCRIPTS" == false ]] && rm -f "$scen_out/subject.jsonl"
done

printf '\n'
"$LIB_DIR/render-report.sh" "$MANIFEST_FILE"
printf '\nResults: %d passed, %d failed, %d errored  (artifacts: %s)\n' \
  "$PASS_COUNT" "$FAIL_COUNT" "$ERROR_COUNT" "$OUT_DIR"

if   [[ $ERROR_COUNT -gt 0 ]]; then exit 2
elif [[ $FAIL_COUNT  -gt 0 ]]; then exit 1
else                                exit 0
fi
