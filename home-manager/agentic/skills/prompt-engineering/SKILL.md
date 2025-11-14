---
name: prompt-engineering
description: "Use when writing or improving any LLM prompts. Applies TDD methodology and research-backed practices (Meincke 2025): test don't assume, measure baseline, iterate rigorously. Prevents assuming universal techniques work. Triggers: 'write a prompt', 'improve prompt', 'prompt not working', general prompting, application development."
---

# Prompt Engineering

## Overview

Prompt engineering is complicated and contingent (Meincke et al. 2025).
Techniques that help in one context may hurt in another.
The solution: test, don't assume.

Core principle: Measure baseline, apply minimal changes, test rigorously.

## When to Use

Required when:

- Writing new prompts for any use case
- Improving existing prompts that underperform
- Creating Claude configurations (skills, agents)
- Developing applications with LLM APIs
- Prompt seems to work inconsistently

Activation trigger phrases:

- "Write a prompt for..."
- "This prompt isn't working"
- "How do I prompt for..."
- "Improve this prompt"
- "The output is inconsistent"

When you think any of these, apply this skill.

## TDD Methodology for Prompts

Test-Driven Development applies to prompts:
observe failures with current approach,
write minimal improvements,
then test rigorously.

### Red Phase: Establish Baseline

Before changing anything, measure current performance.

Required steps:

1. Define success criteria:
   - What does good output look like?
   - How will you measure it?
   - What's the acceptable threshold?

2. Create test cases:
   - Minimum 5-10 representative examples
   - Include edge cases
   - Cover expected input variations

3. Run current prompt and document results:
   - How many test cases pass?
   - What patterns in failures?
   - Where does it struggle?

You cannot proceed to Green without baseline measurements.

Forbidden rationalizations:

- "I'll just try this technique"
- "Step-by-step thinking always helps"
- "Being polite to AI improves output"
- "I'll test after I improve it"
- "This is a simple prompt, no need to test"
- "I know this works from experience"

If you haven't measured baseline, you don't know if changes help.

Failure mode: Skipping Red creates prompts that may work worse than simple baseline.

### Green Phase: Minimal Improvements

Apply one technique at a time.
Test after each change.

#### Core Techniques

**1. Clear, Explicit Instructions** (highest priority)

State exactly what you want.
Modern models respond best to direct communication.

Bad: "Explain climate change"
Good: "Write 3 paragraphs explaining climate change for high school students. Use bullet points for key facts. Maintain neutral tone."

Required elements:
- Task description
- Output format
- Tone/style
- Constraints
- Audience (if relevant)

**2. Examples (Few-Shot)**

Show the model what you want through examples.

Provide 2-5 input-output pairs demonstrating desired behavior.
More examples = better performance, but diminishing returns after ~5.

Format:
```
Example 1:
Input: [example input]
Output: [desired output]

Example 2:
Input: [example input]
Output: [desired output]

Now complete:
Input: [actual input]
Output:
```

**3. Chain of Thought**

For reasoning tasks, ask model to show its work.

Add: "Think through this step-by-step" or "Explain your reasoning"

Note: Meincke et al. found CoT value decreasing with newer models.
Test whether it helps your use case.

**4. Structure and Formatting**

Separate different parts clearly.

Use:
- Clear headings
- Whitespace between sections
- Consistent formatting

Optional: XML tags for very complex prompts or when parsing output programmatically.

**5. Output Format Specification**

Be explicit about format.

For applications: Use SDK schema/configuration when available.
Don't embed JSON schema in prompt if SDK provides native schema support.

For chat: Specify format in instructions ("Respond in bullet points", "Use markdown table").

#### One Change at a Time

Change one thing, measure impact, repeat.

Don't change:
- Instructions AND examples AND format simultaneously

Do change:
- Instructions (test)
- Then add examples (test)
- Then adjust format (test)

This isolates what helps vs hurts.

### Refactor Phase: Rigorous Testing

Single-run testing masks variability.

Meincke et al. showed 60-point swings on individual questions.
Performance collapses at strict thresholds.

Required testing:

1. Run prompt on all test cases (minimum 5-10)
2. For high-stakes applications: Run multiple times per test case
3. Measure:
   - Success rate
   - Consistency across runs
   - Edge case handling

4. Match evaluation to use case:
   - Exploratory tasks: 51% majority-correct may suffice
   - Critical applications: Need higher consistency

Only after rigorous testing can you claim improvement.

## Three Use Cases

### 1. Claude Configuration (Skills/Agents)

When creating skills or agent prompts:

- Apply TDD methodology from this skill
- Use frontmatter for metadata (name, description)
- Structure with clear headings
- Test agent behavior against baseline
- Refine based on observed failures

Note: Specialized guidance for agent documentation exists (quality gates, persuasion principles, token efficiency requirements) but core TDD methodology remains the same.

### 2. Ad-Hoc Prompting

When using chat interfaces:

- Start with clear instructions
- Add examples if output misses the mark
- Refine based on actual responses
- Iterate quickly in conversation

### 3. Application Development

When building applications with LLM APIs:

**Separate concerns:**

- **Prompt content**: Task instructions, examples, context
- **SDK configuration**: Output schema, system prompts, temperature, safety settings

Decision criteria - Use SDK configuration for:
- Output format/schema (JSON structure, field types)
- Generation parameters (temperature, top_k, max_tokens)
- System-level instructions (role, behavior guidelines)
- Safety settings, stop sequences

Use prompt content for:
- Task-specific instructions
- Domain context
- Examples demonstrating desired behavior
- Input data to process

Don't embed in prompt what SDK handles natively.
This improves maintainability and leverages SDK optimizations.

Keep prompts focused on the task, let SDK handle structure.

Example separation:

```
// SDK Configuration
schema = {type: "object", properties: {...}}
systemPrompt = "You are a helpful assistant"
temperature = 0.7

// Prompt Content (focused on task)
userPrompt = "Analyze the following customer review and extract sentiment..."
```

Don't mix these in one blob.

## Context-Dependent Effectiveness

Meincke et al. findings:

1. No universal formulas exist
2. Politeness sometimes helps, sometimes hurts
3. Constraints help in some cases, hurt in others
4. Small wording changes can cause large swings

Implications:

- Test techniques in your context
- Don't copy prompts blindly from internet
- What worked for others may not work for you
- Measure, don't assume

## Red Flags

Stop immediately if you catch yourself thinking:

- "Being polite always helps" → Test it
- "Chain of thought is best practice" → Test it
- "XML tags improve prompts" → Test it
- "This worked on Twitter" → Test in your context
- "I'll embed the schema in the prompt" → Check if SDK supports native schema
- "One test run looks good" → Run multiple test cases
- "Let me try this technique" → Measure baseline first
- "I'll put everything in the prompt" → Separate SDK config from content
- "Simple prompts don't need testing" → All prompts need baseline
- "Let me add multiple improvements" → One change at a time

All of these mean: Stop. Follow TDD methodology.

## Anti-Patterns

Forbidden without testing:

- Applying multiple techniques simultaneously
- Assuming universal best practices
- Single-shot evaluation
- Copying prompts without validation
- Embedding structure in prompt when SDK handles it
- Changing prompts without baseline measurement

Required before claiming success:

- Test on multiple cases
- Measure consistency
- Compare to baseline
- Document what actually improved

## Integration with Other Skills

- verification-before-completion: Test prompts before claiming they work
- systematic-debugging: When prompts fail, investigate systematically

## Examples

### Good: TDD Approach

```
User: I need a prompt to summarize customer reviews.
Agent: Let me start by establishing a baseline.

First, what are your success criteria?
- How long should summaries be?
- What information must be included?
- Who is the audience?

[User provides criteria]

Agent: Let me create 5 test reviews covering different scenarios.
I'll run a simple prompt first to measure baseline performance.

[Tests baseline, documents results]

Agent: Baseline passes 2/5 test cases. Common failure: missing sentiment.
Let me add explicit instruction to include sentiment.

[Tests improved version]

Agent: Now passes 4/5. Remaining failure is edge case with sarcasm.
Adding example of sarcastic review handling.

[Tests final version]

Agent: Passes 5/5 test cases. Ready for your review.
```

### Bad: Assumption-Based Approach

```
User: I need a prompt to summarize customer reviews.
Agent: Here's a prompt using best practices:

<instructions>Please think step-by-step and carefully...</instructions>
<examples>...</examples>

I used XML tags, chain of thought, and politeness - these are proven techniques.

[No baseline, no testing, assumed techniques work]
```

## LLM Anti-Pattern Detection

If you output these phrases before testing:

- "Here's a prompt using best practices..." → Test it first
- "Chain of thought will improve..." → Measure baseline
- "Being polite helps..." → Prove it
- "XML tags make this clearer..." → Test with and without
- "This should work..." → Run test cases

When detected: Stop, measure baseline, test rigorously, then proceed.
