---
name: product-definition
description: "Use when creating Product Requirements Documents. Enforces clarification-first workflow, junior-dev-friendly language, and standard PRD structure. Triggers: 'create PRD', 'write requirements', 'product requirements', feature requests needing documentation."
---

# Product Definition

Required when creating Product Requirements Documents (PRD) for feature requests.
Enforces structured clarification before documentation.

## Workflow: Clarify Then Document

You must follow this sequence:

1. **Receive Request** - User describes desired feature
2. **Ask Questions** - Required clarification phase (see below)
3. **Generate PRD** - Create document using required structure
4. **Get Approval** - Never begin implementation before user approval

Never skip the clarification phase.
Never start implementation before approval.

## Clarification Phase

Ask 3-5 critical questions before writing any PRD.

Required question areas:

- Problem definition (what problem does this solve?)
- Core functionality (what must this feature do?)
- Scope boundaries (what should this NOT include?)
- Success indicators (how will we measure success?)

### Question Format

Use numbered questions with lettered options:

```
1. What is the primary problem users face?
   A. They cannot prioritize tasks effectively
   B. They struggle to find tasks in large lists
   C. They need better collaboration features

2. Which prioritization method should we support initially?
   A. Simple high/medium/low levels
   B. Eisenhower Matrix (urgent/important)
   C. Weighted scoring with custom factors
```

Users respond with selections like: "1A, 2C"

### Question Guidelines

Only ask questions where answers are not reasonably inferable from the user's initial request.

Do not ask:

- Questions answered in the original prompt
- Obvious implementation details
- Preferences you can determine from context

## PRD Structure

Required sections in this order:

### 1. Introduction

Explain the problem and goal.
Target audience: junior developers who will implement this.
Use clear, simple language.
Avoid technical jargon.

### 2. Objectives

List specific, measurable goals.
Each objective should be testable.

### 3. User Stories

Describe how users will interact with the feature.
Format: "As a \[user type], I want to \[action] so that \[benefit]"

Include realistic usage scenarios.

### 4. Functional Requirements

Use numbered "The system must..." statements.

Example:

```
1. The system must allow users to assign priority levels to tasks
2. The system must display tasks sorted by priority by default
3. The system must recalculate priority when task attributes change
```

Never use "shall", "should", or "may" in requirements.
Use "must" for all functional requirements.

### 5. Out of Scope

Explicitly list what this feature will NOT include.
Prevents scope creep.
Helps junior developers understand boundaries.

Example:

```
Out of scope for this release:
- AI-powered priority suggestions
- Team-wide priority templates
- Mobile-specific priority gestures
```

### 6. Design Considerations (Optional)

High-level UI/UX guidance if relevant.
Avoid technical implementation details.

### 7. Success Criteria

How will we know this feature works?
Include measurable metrics.

Example:

```
- 60% of users assign priorities within first week
- Task selection time decreases by 30%
- Zero priority-related bugs in first month
```

### 8. Open Questions

List anything still unclear after clarification phase.
These must be resolved before implementation begins.

## Language Requirements

Target junior developers as primary readers.

Required:

- Simple, declarative sentences
- Plain English (avoid: "utilize", "leverage", "facilitate")
- Concrete examples over abstract concepts
- Explicit over implicit

Forbidden:

- Technical jargon without explanation
- Code snippets in requirements
- Vague terms ("user-friendly", "intuitive", "seamless")
- Passive voice ("shall be provided", "is enabled")

## Common Mistakes

**Over-specification**
Do not include:

- Competitive analysis
- Technical architecture
- Database schemas
- API specifications
- Implementation timelines

These belong in separate technical design documents.

**Under-specification**
Every requirement must be:

- Testable (can verify it works)
- Unambiguous (one interpretation only)
- Necessary (supports stated objectives)

**Skipping Clarification**
If you write a PRD without asking questions first, you have failed.
The clarification phase is mandatory.

## Validation Checklist

Before saving PRD, verify:

- [ ] Asked 3-5 clarification questions first
- [ ] All required sections present
- [ ] Requirements use "The system must..." format
- [ ] "Out of Scope" section included
- [ ] Language appropriate for junior developers
- [ ] No technical jargon without explanation
- [ ] Asked user for approval before implementation

## After PRD Creation

State: "PRD complete. Please review and approve before I begin implementation."

Wait for explicit approval.
Do not proceed with any coding, architecture, or design work until user confirms.
