---
id: 01-ping
title: Smoke test - ping pong
fixture: fixtures/_empty
expect_trigger: true
target_lens: null
category: pressure
assertions:
  - id: assistant-said-pong
    text: The assistant emits text containing PONG.
  - id: no-tool-calls
    text: The transcript contains no toolCall blocks.
---

ping
