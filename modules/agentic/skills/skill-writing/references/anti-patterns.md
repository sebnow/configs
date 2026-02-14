# Anti-Patterns in Skill Writing

Never:

- Use XML angle brackets (< >) in frontmatter (enables prompt injection via system prompt)
- Write documentation without observing baseline failures
- Use vague names ("Helper", "Utils") instead of gerunds ("Processing PDFs")
- Use first person in descriptions (breaks discoverability)
- Nest file references deeply (prevents complete reads)
- Offer many options without clear defaults
- Include magic numbers without justification
- Use Windows-style paths (always use forward slashes)
- Defer to agents when scripts solve problems
- Include ALL CAPS (except acronyms) or emojis
- Write descriptions in first person
- Create deeply nested file hierarchies
- Place files at skill root besides SKILL.md
- Use reserved name prefixes ("claude", "anthropic")
- Omit error handling or troubleshooting section in skill body
- Omit examples in skill body
- Write overly verbose instructions (causes non-compliance)
- Bury critical instructions below less important content
- Use ambiguous validation language
  ("make sure" instead of specific checks like "Before calling X, verify Y")
- Omit negative triggers when skill could over-trigger on related topics
