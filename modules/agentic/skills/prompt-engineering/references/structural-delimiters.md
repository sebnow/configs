# Structural Delimiters: XML vs Markdown

## Primary Use Case: Context Bleed

Context bleed is when rules from one section of an instruction document
apply in contexts they shouldn't.
XML tags are the primary fix.

Diagnostic: if section A and section B coexist in an instruction file
and the model applies A's rules during B's tasks, scope section A with XML tags.

Models do not suggest XML tags unprompted when asked to fix context bleed.
Apply proactively once the failure mode is confirmed.

## Model Preferences

Architectural preferences differ by model family.
Test your specific context regardless.

### Claude (Anthropic — all models)

Anthropic (current docs):

> "XML tags help Claude parse complex prompts unambiguously,
> especially when your prompt mixes instructions, context, examples,
> and variable inputs. Wrapping each type of content in its own tag
> reduces misinterpretation."

Trained on XML-structured prompts; the format is privileged architecturally.
Consistent across Haiku, Sonnet, and Opus.

### GPT-4.x (OpenAI)

OpenAI GPT-4.1 prompting guide:

> "We recommend starting with Markdown titles for major sections and subsections.
> [XML] also performs well, and we have improved adherence to information
> in XML with this model."
> "JSON performed particularly poorly."

Markdown-first; XML a strong alternative. XML preferred over JSON for long context.

### Gemini (Google)

Google Vertex AI docs: XML-style tags or Markdown headings both acceptable.
No architectural preference stated.
Guidance: pick one format and apply it consistently.

## When to Use XML Tags

- Section's rules must not bleed into other contexts
- Mixing instructions, data, and examples in a single prompt
- Long context (20k+ tokens)
- Parsing output programmatically

## When Markdown Suffices

- Single-purpose prompts without distinct sections
- Primarily targeting GPT or Gemini without Claude cross-targeting
- Short prompts where misinterpretation risk is low

## Rendering Caveat for Markdown Files

XML tags in `.md` files affect rendering:

- GitHub sanitizes unknown tags; content is preserved, tags are stripped
- CommonMark: opening tag with no blank line after it starts a raw HTML block —
  Markdown inside is **not** parsed
- Fix: add a blank line after every opening tag and before every closing tag
