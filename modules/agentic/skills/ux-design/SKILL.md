---
name: ux-design
description: "Use when designing user interfaces, improving UX, or creating design specifications. Enforces user-centered process, WCAG 2.1 AA accessibility, mobile-first approach. Triggers: 'design interface', 'improve UX', 'user experience', 'accessibility', 'responsive design', 'user flow'."
---

# UX Design

Required when creating or improving user interfaces.
Enforces user-centered design with accessibility-first approach.

## Core Principles

You must follow these non-negotiable principles:

1. **User needs drive decisions** - Never add features without user validation
2. **Accessibility is baseline** - WCAG 2.1 AA minimum, AAA where feasible
3. **Mobile-first always** - Design for constraints, enhance for capabilities
4. **Document rationale** - Every decision needs recorded reasoning
5. **Test with users** - Assumptions kill usability

## Mandatory Workflow

Never skip steps. Each phase validates the next.

### Phase 1: Research (Required)

Before any design work:

1. Identify user segments and needs
2. Define success metrics
3. Document constraints (technical, time, resources)
4. Map current pain points if redesigning

State findings explicitly before proceeding.

### Phase 2: Information Architecture

Create structure before visuals:

1. User flows (happy path + edge cases)
2. Content hierarchy
3. Navigation patterns
4. Error state handling

Use simple diagrams or structured text.
Never jump to visual design without IA.

### Phase 3: Design Implementation

#### Accessibility Requirements (Non-Negotiable)

**Perceivable**
- Color contrast: 4.5:1 text, 3:1 UI components
- Alt text for all informative images
- Captions/transcripts for media
- Text resizable to 200% without horizontal scroll

**Operable**
- Keyboard navigable (visible focus indicators)
- Touch targets minimum 44x44px (48x48px preferred)
- No keyboard traps
- Sufficient time limits
- Skip navigation links

**Understandable**
- Clear labels and instructions
- Consistent navigation
- Error identification and recovery guidance
- Plain language (8th grade level default)

**Robust**
- Semantic HTML structure
- ARIA only when native HTML insufficient
- Progressive enhancement

#### Mobile-First Implementation

Start with smallest viewport (320px).
Use min-width queries to enhance.

```css
/* Correct mobile-first */
.container { width: 100%; }
@media (min-width: 768px) { .container { width: 750px; } }

/* Never use max-width for responsive */
```

#### Design Tokens (Required)

Follow ui-design skill for systematic design token implementation.

Required token categories: color, spacing, typography, border radius, shadows.

### Phase 4: Validation

Required before implementation:

1. Accessibility audit against WCAG 2.1 AA
2. Responsive testing (320px to 2560px)
3. User flow verification
4. Performance budget check
5. Cross-browser/device testing plan

### Phase 5: Documentation

Create implementation specifications:

1. Component inventory with states
2. Interaction patterns
3. Responsive breakpoints
4. Accessibility annotations
5. Edge case handling
6. Success metrics

## Avoiding Generic Design

Combat "AI slop" through specificity:

**Typography**: Choose distinctive fonts with high weight contrast.
Never default to Inter/Roboto without justification.

**Color**: Commit to bold palettes.
Avoid purple gradients and predictable combinations.

**Layout**: Break from centered containers.
Use asymmetry and white space deliberately.

**Motion**: Prioritize meaningful transitions.
Stagger page loads, avoid decoration.

**Personality**: Match brand voice.
Generic "clean" and "modern" are not design directions.

## Red Flags to Prevent

Never proceed if you catch yourself:

- Adding features without user need
- Treating accessibility as enhancement
- Starting with desktop layouts
- Using placeholder "Lorem ipsum" content
- Copying common patterns without adaptation
- Skipping documentation
- Assuming all users are identical
- Designing without constraints

## Component Patterns

See @components.md for reusable patterns.

## Testing Methodology

See @testing.md for validation protocols.

## Decision Framework

When facing choices:

1. What do users actually need? (not want)
2. What's the simplest solution?
3. How might this fail for disabled users?
4. What's the mobile experience?
5. How do we measure success?

Document answers before proceeding.

## Progressive Disclosure

Complex features require staged revelation:
- Core functionality visible immediately
- Advanced options revealed on demand
- Help text contextual, not overwhelming

## Error Prevention

Design to prevent errors before handling them:
- Clear affordances
- Confirmation for destructive actions
- Inline validation
- Progressive form disclosure
- Smart defaults

## Performance Budget

- Initial load: <3 seconds on 3G
- Time to interactive: <5 seconds
- Core Web Vitals: Pass all thresholds
- Images: Optimized and lazy-loaded
- Fonts: Subset and preloaded

## Deliverables Checklist

Before marking complete:

- [ ] User research documented
- [ ] User flows mapped
- [ ] Wireframes/mockups created
- [ ] Design tokens defined
- [ ] Accessibility audit passed
- [ ] Responsive behavior specified
- [ ] Component states documented
- [ ] Interaction patterns defined
- [ ] Implementation notes written
- [ ] Success metrics established

State: "UX design phase complete" only after all items checked.