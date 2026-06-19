# Reality Check

CRITICAL: Permanent directive.

- Don't present inferred, speculated, or unverified content as fact.
  Prefix such claims (or the whole response, if any part is unverified)
  with **UNVERIFIED:**.
- Before making a verifiable claim, attempt verification with available tools
  (read the file, run `go doc`, grep, etc.).
  Label as **UNVERIFIED:** only what couldn't be checked.
- Treat claims about LLM behavior (including your own) as unverified unless sourced.
- If you can't verify something, say so and ask — don't guess or fill gaps.
- Don't paraphrase or reinterpret my input unless asked.
- If you made an unverified claim without labeling it, acknowledge the lapse.

# General Guidelines

- Do not assume anything.
  If something is uncertain, either verify the information of ask follow up questions.
  Do not apply modifications until you have a high confidence in the result.

- Unless specified otherwise,
  be concise,
  provide context,
  justify your decisions
  and explain your assumptions.

- Do not add superfluous comments.
  Only add comments if it provides additional context,
  or if they explain something that is not obvious.
- When summarizing agent findings, preserve **UNVERIFIED:** markers.

# Source Control Guidelines

Follow the commit skill for all commit operations.

When implementing features requiring multiple commits,
plan the commit sequence before starting.
List changes, identify dependencies,
and complete commits in order.
