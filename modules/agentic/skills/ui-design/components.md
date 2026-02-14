# Component Library Patterns

Systematic patterns for building consistent, maintainable component libraries.

## Component Token System

Each component must define its own tokens:

```css
/* Button component tokens */
.button {
  --button-padding-y: var(--space-2);
  --button-padding-x: var(--space-4);
  --button-radius: var(--radius-md);
  --button-font-size: var(--text-sm);
  --button-font-weight: var(--font-weight-semibold);
  --button-line-height: 1;

  /* Color tokens */
  --button-bg: var(--color-primary);
  --button-bg-hover: var(--color-primary-dark);
  --button-text: var(--color-white);
  --button-border: transparent;
  --button-border-focus: var(--color-primary-light);
}

/* Variant tokens */
.button--secondary {
  --button-bg: var(--color-secondary);
  --button-bg-hover: var(--color-secondary-dark);
}

.button--outline {
  --button-bg: transparent;
  --button-text: var(--color-primary);
  --button-border: var(--color-primary);
}
```

## Component States

Document all interactive states:

```css
.button {
  /* Default */
  background: var(--button-bg);
  color: var(--button-text);
  border: 1px solid var(--button-border);
  padding: var(--button-padding-y) var(--button-padding-x);
  border-radius: var(--button-radius);
  font-size: var(--button-font-size);
  font-weight: var(--button-font-weight);
  transition: all 0.2s var(--ease-out);

  /* Hover */
  &:hover {
    background: var(--button-bg-hover);
    transform: translateY(-1px);
    box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
  }

  /* Active */
  &:active {
    transform: translateY(0);
    box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
  }

  /* Focus (keyboard) */
  &:focus-visible {
    outline: 2px solid var(--button-border-focus);
    outline-offset: 2px;
  }

  /* Disabled */
  &:disabled {
    opacity: 0.5;
    cursor: not-allowed;
    pointer-events: none;
  }

  /* Loading */
  &[data-loading="true"] {
    position: relative;
    color: transparent;
    pointer-events: none;
  }

  &[data-loading="true"]::after {
    content: '';
    position: absolute;
    width: 16px;
    height: 16px;
    top: 50%;
    left: 50%;
    margin: -8px 0 0 -8px;
    border: 2px solid currentColor;
    border-radius: 50%;
    border-top-color: transparent;
    animation: rotate 0.6s linear infinite;
  }
}
```

## Component Variants

Use data attributes or classes for variants:

```css
/* Size variants */
.button--sm {
  --button-padding-y: var(--space-1);
  --button-padding-x: var(--space-3);
  --button-font-size: var(--text-xs);
}

.button--lg {
  --button-padding-y: var(--space-3);
  --button-padding-x: var(--space-6);
  --button-font-size: var(--text-lg);
}

/* Style variants */
.button--ghost {
  --button-bg: transparent;
  --button-bg-hover: var(--color-gray-100);
  --button-text: var(--color-text);
  --button-border: transparent;
}

.button--danger {
  --button-bg: var(--color-error);
  --button-bg-hover: var(--color-error-dark);
}
```

## Design System Tools

### Style Dictionary

Generate platform-specific tokens from JSON:

**tokens.json:**
```json
{
  "color": {
    "primary": {
      "value": "#3b82f6",
      "type": "color"
    },
    "text": {
      "body": {
        "value": "#1f2937",
        "type": "color"
      }
    }
  },
  "space": {
    "4": {
      "value": "1rem",
      "type": "dimension"
    }
  }
}
```

**config.js:**
```javascript
module.exports = {
  source: ['tokens/**/*.json'],
  platforms: {
    css: {
      transformGroup: 'css',
      buildPath: 'build/css/',
      files: [{
        destination: 'tokens.css',
        format: 'css/variables'
      }]
    },
    js: {
      transformGroup: 'js',
      buildPath: 'build/js/',
      files: [{
        destination: 'tokens.js',
        format: 'javascript/es6'
      }]
    }
  }
};
```

### Tailwind CSS 4 @theme

Define tokens once, use everywhere:

```css
@theme {
  /* Colors */
  --color-primary: #3b82f6;
  --color-secondary: #8b5cf6;

  /* Spacing */
  --space-1: 0.25rem;
  --space-2: 0.5rem;
  --space-4: 1rem;

  /* Typography */
  --font-sans: system-ui, sans-serif;
  --text-base: 1rem;
  --text-lg: 1.25rem;

  /* Radius */
  --radius-sm: 0.25rem;
  --radius-md: 0.5rem;
  --radius-lg: 1rem;
}
```

Usage in HTML:

```html
<button class="bg-primary text-white px-4 py-2 rounded-md">
  Click me
</button>
```

## Component Composition

Build complex components from primitives:

```css
/* Primitive: Box */
.box {
  display: block;
  padding: var(--box-padding, var(--space-4));
  border-radius: var(--box-radius, var(--radius-md));
  background: var(--box-bg, var(--color-surface));
}

/* Composed: Card */
.card {
  --box-padding: var(--space-6);
  --box-radius: var(--radius-lg);
  --box-bg: var(--color-surface);

  box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
  transition: box-shadow 0.3s var(--ease-out);
}

.card:hover {
  box-shadow: 0 12px 24px rgba(0, 0, 0, 0.15);
}
```

## Semantic HTML

Always use appropriate HTML elements:

```html
<!-- Good -->
<button type="button" class="button">
  Click me
</button>

<a href="/page" class="button">
  Go to page
</a>

<!-- Bad -->
<div class="button" onclick="...">
  Click me
</div>
```

## ARIA Patterns

Add ARIA when semantic HTML is insufficient:

```html
<!-- Toggle button -->
<button
  type="button"
  aria-pressed="false"
  aria-label="Toggle dark mode"
  class="toggle"
>
  <span class="toggle-icon"></span>
</button>

<!-- Loading button -->
<button
  type="submit"
  aria-busy="true"
  aria-label="Saving changes"
  class="button"
  data-loading="true"
>
  Save
</button>

<!-- Disclosure widget -->
<button
  type="button"
  aria-expanded="false"
  aria-controls="panel-1"
  class="disclosure-button"
>
  Show details
</button>
<div id="panel-1" hidden>
  Panel content
</div>
```

## Component Documentation Template

Document each component systematically:

```markdown
## Button

### Purpose
Primary interactive element for triggering actions.

### Variants
- `button--primary`: Main call-to-action
- `button--secondary`: Secondary actions
- `button--outline`: Tertiary actions
- `button--ghost`: Minimal emphasis

### Sizes
- `button--sm`: Compact layouts
- `button--md`: Default size
- `button--lg`: Prominent actions

### States
- Default
- Hover
- Active
- Focus (keyboard)
- Disabled
- Loading

### Props/Attributes
- `type`: "button" | "submit" | "reset"
- `disabled`: boolean
- `data-loading`: boolean
- `aria-label`: string (if no text content)

### Usage
\```html
<button type="button" class="button button--primary">
  Primary action
</button>
\```

### Accessibility
- Uses semantic `<button>` element
- Visible focus indicator
- Disabled state prevents interaction
- Loading state communicates with aria-busy
```

## Component Library Checklist

Before marking component complete:

- [ ] All states documented (default, hover, active, focus, disabled)
- [ ] Component tokens defined
- [ ] Variants implemented consistently
- [ ] Semantic HTML used
- [ ] Keyboard accessible
- [ ] Focus indicators visible
- [ ] ARIA attributes added where needed
- [ ] Responsive behavior defined
- [ ] Dark mode support
- [ ] Performance tested
- [ ] Documentation written

## Testing Components

Verify across:
- Keyboard navigation
- Screen readers
- Touch devices
- Different viewport sizes
- Light and dark modes
- High contrast mode
- Reduced motion preferences
