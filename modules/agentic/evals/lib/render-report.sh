#!/usr/bin/env bash
set -euo pipefail

# Render a pass/fail table from the run manifest.
# Usage: render-report.sh <manifest.jsonl>

MANIFEST_FILE="${1:?Usage: render-report.sh <manifest.jsonl>}"
[[ -f "$MANIFEST_FILE" ]] || {
  printf 'error: manifest not found: %s\n' "$MANIFEST_FILE" >&2; exit 2
}

printf '%-7s  %-32s  %s\n' "STATUS" "ID" "TITLE"
printf '%0.s-' {1..72}; printf '\n'

while IFS= read -r line; do
  status=$(jq -r '.status' <<< "$line")
  sid=$(jq -r '.id' <<< "$line")
  stitle=$(jq -r '.title // ""' <<< "$line")

  case "$status" in
    PASS)
      printf 'PASS     %-32s  %s\n' "$sid" "$stitle"
      ;;
    FAIL)
      printf 'FAIL     %-32s  %s\n' "$sid" "$stitle"
      verdict_file=$(jq -r '.verdict_file // ""' <<< "$line")
      if [[ -n "$verdict_file" && -f "$verdict_file" ]]; then
        while IFS= read -r al; do
          aid=$(jq -r '.id' <<< "$al")
          reason=$(jq -r '.reason' <<< "$al")
          printf '           FAIL [%s]: %s\n' "$aid" "$reason"
        done < <(jq -c '.assertions[] | select(.status == "FAIL")' "$verdict_file")
      fi
      ;;
    ERROR)
      errmsg=$(jq -r '.error // "unknown"' <<< "$line")
      printf 'ERROR    %-32s  [%s]\n' "$sid" "$errmsg"
      ;;
    *)
      printf '%-7s  %-32s  %s\n' "$status" "$sid" "$stitle"
      ;;
  esac
done < "$MANIFEST_FILE"
