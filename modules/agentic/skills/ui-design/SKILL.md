---
name: ui-design
description: "Use when implementing visual design, styling components, or building design systems. Enforces intentional aesthetics, systematic design tokens, production-grade implementation. Triggers: 'implement design', 'style component', 'design system', 'design tokens', 'visual design', 'component library', 'frontend styling'."
---

# UI Design

Required when implementing visual design and styling interfaces.
Enforces intentional aesthetics and systematic implementation to avoid generic patterns.

## Core Principles

You must follow these non-negotiable principles:

1. **Intentionality over intensity** - Bold maximalism and refined minimalism both work; generic mediocrity fails
2. **Design tokens are mandatory** - Systematic values prevent inconsistency
3. **Avoid AI slop aesthetics** - Generic patterns destroy brand identity
4. **Implementation matches vision** - Maximalist designs require elaborate code; minimalist designs demand precision
5. **AI personalization requires ethics** - Transparency and user control are non-negotiable

## Mandatory Pre-Implementation

Never start coding without defining:

### Aesthetic Direction (Required)

Commit to a bold aesthetic choice before implementation:

**Tone Options:**
- Brutalist (raw, functional, high contrast)
- Maximalist (layered, rich, abundant detail)
- Retro-futuristic (nostalgic tech, bold geometry)
- Organic (natural forms, flowing lines)
- Luxury (refined, elegant, spacious)
- Playful (unexpected, whimsical)
- Editorial (typographic hierarchy, grid-based)
- Art deco (geometric glamour, bold symmetry)
- Industrial (mechanical, utilitarian)
- Soft (gentle curves, muted tones)

State your chosen aesthetic and commit to it throughout implementation.

Generic "clean" and "modern" are not acceptable design directions.

### Technical Context

Document before proceeding:

1. Framework/library constraints
2. Performance budget
3. Browser/device support requirements
4. Existing design system (if any)
5. AI personalization needs

## Design Token System (Non-Negotiable)

You must implement systematic design tokens.
Never use magic numbers or arbitrary values.

### Token Structure

Use three-tier system:
1. **Foundation tokens**: Raw values (#3b82f6, 0.25rem)
2. **Semantic tokens**: Purpose-driven (--color-primary, --space-base)
3. **Component tokens**: Specific usage (--button-color-primary-bg)

### Naming Convention

Use: `category-property-variant-state` structure.

Examples: `--color-text-body`, `--button-color-primary-hover`, `--space-stack-large`

### Required Categories

- **Color**: primary, secondary, accent, text, background, surface, border, states
- **Spacing**: 8px base system (space-1 through space-24)
- **Typography**: Consistent scale (1.25 or 1.414 ratio)
- **Border radius**: sm, md, lg, xl, full
- **Shadows**: sm, md, lg, xl, 2xl

See @tokens.md for detailed token definitions and platform-specific generation.

## Avoiding Generic AI Aesthetics

These patterns signal low-quality AI-generated design.

### Forbidden Patterns

**Typography:** Inter/Roboto without justification, single font weight, uniform sizing
**Color:** Purple-blue gradients by default, pastel without contrast, gray-on-gray-on-gray
**Layout:** Everything centered equally, only card grids, no asymmetry, uniform spacing
**Motion:** Decorative animations, simultaneous animations, fade-in only, no easing

### Required Distinctiveness

**Typography:** Distinctive fonts with personality, contrasting weights (300/400 body, 600/700/800 headings), variable fonts
**Color:** Dominant color + sharp accents, intentional contrast levels, documented usage
**Composition:** Asymmetry, overlap for depth, diagonal flow, grid-breaking moments, negative space as design element

## Frontend Implementation Guidelines

### Typography Implementation

Use distinctive fonts with proper hierarchy.
Never default to Inter/Roboto without justification.

Required elements:
- Font pairing (display + body)
- Weight hierarchy (300/400 for body, 600/700/800 for headings)
- Variable fonts for fluid control
- Proper line-height and letter-spacing

See @typography.md for implementation patterns and font loading.

### Color & Theme Implementation

Use CSS custom properties for theme switching.
Map semantic tokens to foundation tokens per theme.

Required contrast ratios:
- Text: 4.5:1 minimum (WCAG AA)
- Interactive components: 3:1 minimum

See @tokens.md for dark mode implementation patterns.

### Motion & Animation

Prioritize high-impact moments over scattered effects.

Required:
- Proper easing curves (never linear)
- Respect prefers-reduced-motion
- Performance-conscious implementation
- Purposeful animations only

See @animation.md for orchestration patterns and scroll-triggered effects.

### Background & Atmosphere

Create visual richness through layering when aesthetic requires it.
See @backgrounds.md for gradient meshes, textures, and patterns.

## Component Library Patterns

Each component must:
- Define its own tokens
- Document all interactive states (default, hover, active, focus, disabled)
- Use semantic HTML
- Include keyboard focus indicators

See @components.md for implementation patterns and design system tools.

## AI-Driven Personalization

When implementing AI-powered interface adaptation:

### Ethical Requirements (Non-Negotiable)

1. **Transparency:** Users must know when AI personalizes their experience
2. **Control:** Provide opt-out and reset options
3. **Explanation:** Show why adaptations occurred
4. **Human fallback:** Offer support when AI fails
5. **Privacy:** Never personalize with sensitive data without explicit consent

### Personalization Boundaries

**Appropriate:**
- Layout density preferences
- Color scheme (light/dark mode)
- Content prioritization
- Navigation shortcuts

**Forbidden:**
- Hiding critical functionality
- Removing accessibility features
- Changing core workflows without notice
- Personalizing based on protected attributes

See @personalization.md for implementation patterns.

## Implementation Checklist

Before marking UI design complete:

- [ ] Aesthetic direction defined and documented
- [ ] Design tokens implemented systematically
- [ ] All token categories defined (color, spacing, typography)
- [ ] No magic numbers in styles
- [ ] Typography uses distinctive fonts with weight hierarchy
- [ ] Color system is cohesive and intentional
- [ ] Motion uses proper easing curves
- [ ] prefers-reduced-motion respected
- [ ] Component states documented (default, hover, active, focus, disabled)
- [ ] Keyboard focus indicators visible
- [ ] Design avoids generic AI patterns
- [ ] AI personalization (if used) includes transparency and control
- [ ] Visual hierarchy is clear
- [ ] Spacing system is consistent

## Red Flags

Stop if you catch yourself:
- Using generic fonts (Inter/Roboto) or purple gradients without justification
- Single font weight, uniform spacing, everything centered
- Magic numbers instead of tokens
- Animating everything simultaneously, skipping component states
- AI personalization without consent UI
- Copying patterns without adaptation

## Deliverables

State: "UI design implementation complete" only after checklist passes.

Document for handoff:
1. Design token definitions
2. Component inventory with all states
3. Aesthetic direction rationale
4. Typography system (fonts, weights, scales)
5. Color system (palette, usage, contrast ratios)
6. Motion principles (easing, duration, triggers)
7. AI personalization boundaries (if applicable)
