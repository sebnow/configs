#!/usr/bin/env bash
set -euo pipefail

# Flatten a pi session JSONL into a human-readable transcript.
# Uses pi's schema (.message.content[], toolCall, toolResult roles).
# toolResult text is truncated to 300 characters per entry.

JSONL="${1:?Usage: extract-transcript.sh <session.jsonl>}"

jq -r '
  select(.type == "message") |
  .message as $msg |
  ($msg.content // [])[] |
  if ($msg.role == "user" and .type == "text") then
    "[user]\n\(.text)\n"
  elif ($msg.role == "assistant" and .type == "text") then
    "[assistant]\n\(.text)\n"
  elif ($msg.role == "assistant" and .type == "toolCall") then
    "[toolCall] \(.name)(\(.arguments | tostring | .[0:200]))\n"
  elif ($msg.role == "toolResult" and .type == "text") then
    "[toolResult] \(.text | .[0:300])\n"
  else
    empty
  end
' "$JSONL"
