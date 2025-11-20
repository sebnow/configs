# Design Tokens

Detailed guidance for implementing systematic design token systems.

## Token Structure

Follow W3C Design Tokens specification for cross-platform consistency.

### Three-Tier System

```css
/* Foundation Tokens - Raw Values */
--foundation-color-blue-500: #3b82f6;
--foundation-space-base: 0.25rem;
--foundation-font-sans: system-ui, -apple-system, sans-serif;

/* Semantic Tokens - Purpose-Driven */
--color-primary: var(--foundation-color-blue-500);
--color-text-body: var(--foundation-color-gray-900);
--color-surface-base: var(--foundation-color-white);

/* Component Tokens - Specific Usage */
--button-color-primary-bg: var(--color-primary);
--button-color-primary-text: var(--color-text-inverse);
--button-padding-x: calc(var(--foundation-space-base) * 4);
```

## Naming Convention

Use: `category-property-variant-state` structure.

Examples:
- `--color-text-body`
- `--space-stack-large`
- `--button-color-primary-hover`
- `--typography-heading-1-size`

## Required Token Categories

### Color Tokens

```css
/* Semantic color system */
--color-primary: /* brand primary */;
--color-secondary: /* brand secondary */;
--color-accent: /* attention/action */;
--color-text: /* primary text */;
--color-text-subtle: /* secondary text */;
--color-background: /* base background */;
--color-surface: /* elevated surfaces */;
--color-border: /* dividers */;
--color-error: /* error states */;
--color-success: /* success states */;
--color-warning: /* warning states */;
```

### Spacing Tokens

8px base system recommended:

```css
--space-1: 0.25rem; /* 4px */
--space-2: 0.5rem;  /* 8px */
--space-3: 0.75rem; /* 12px */
--space-4: 1rem;    /* 16px */
--space-6: 1.5rem;  /* 24px */
--space-8: 2rem;    /* 32px */
--space-12: 3rem;   /* 48px */
--space-16: 4rem;   /* 64px */
--space-20: 5rem;   /* 80px */
--space-24: 6rem;   /* 96px */
```

### Typography Tokens

1.25 ratio (Major Third):

```css
--text-xs: 0.64rem;    /* 10px */
--text-sm: 0.8rem;     /* 13px */
--text-base: 1rem;     /* 16px */
--text-lg: 1.25rem;    /* 20px */
--text-xl: 1.563rem;   /* 25px */
--text-2xl: 1.953rem;  /* 31px */
--text-3xl: 2.441rem;  /* 39px */
--text-4xl: 3.052rem;  /* 49px */
```

1.414 ratio (Augmented Fourth) for more dramatic contrast:

```css
--text-xs: 0.707rem;
--text-sm: 0.841rem;
--text-base: 1rem;
--text-lg: 1.414rem;
--text-xl: 2rem;
--text-2xl: 2.827rem;
--text-3xl: 4rem;
--text-4xl: 5.657rem;
```

### Border Radius Tokens

```css
--radius-none: 0;
--radius-sm: 0.125rem;  /* 2px */
--radius-md: 0.375rem;  /* 6px */
--radius-lg: 0.5rem;    /* 8px */
--radius-xl: 0.75rem;   /* 12px */
--radius-2xl: 1rem;     /* 16px */
--radius-full: 9999px;
```

### Shadow Tokens

```css
--shadow-sm: 0 1px 2px 0 rgba(0, 0, 0, 0.05);
--shadow-md: 0 4px 6px -1px rgba(0, 0, 0, 0.1);
--shadow-lg: 0 10px 15px -3px rgba(0, 0, 0, 0.1);
--shadow-xl: 0 20px 25px -5px rgba(0, 0, 0, 0.1);
--shadow-2xl: 0 25px 50px -12px rgba(0, 0, 0, 0.25);
```

## Platform-Specific Token Generation

### Style Dictionary

Generate tokens for multiple platforms from JSON source.

**tokens.json:**
```json
{
  "color": {
    "primary": {
      "value": "#3b82f6",
      "type": "color"
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
    },
    ios: {
      transformGroup: 'ios',
      buildPath: 'build/ios/',
      files: [{
        destination: 'tokens.swift',
        format: 'ios-swift/class.swift'
      }]
    },
    android: {
      transformGroup: 'android',
      buildPath: 'build/android/',
      files: [{
        destination: 'tokens.xml',
        format: 'android/resources'
      }]
    }
  }
};
```

### Tailwind CSS 4 @theme

Define tokens once in CSS:

```css
@theme {
  /* Colors */
  --color-primary: #3b82f6;
  --color-secondary: #8b5cf6;
  --color-accent: #ec4899;

  /* Spacing */
  --space-1: 0.25rem;
  --space-2: 0.5rem;
  --space-4: 1rem;
  --space-8: 2rem;

  /* Typography */
  --font-sans: system-ui, sans-serif;
  --font-serif: Georgia, serif;
  --text-base: 1rem;
  --text-lg: 1.25rem;

  /* Radius */
  --radius-sm: 0.25rem;
  --radius-md: 0.5rem;
  --radius-lg: 1rem;
}
```

Tailwind automatically generates utilities:

```html
<button class="bg-primary text-white px-4 py-2 rounded-md">
  Click me
</button>
```

## Token Organization Strategies

### Single File

Simple projects:

```css
/* tokens.css */
:root {
  /* All tokens here */
}
```

### Category Files

Medium projects:

```
tokens/
  colors.css
  spacing.css
  typography.css
  shadows.css
```

### Tier-Based Files

Large design systems:

```
tokens/
  foundation/
    colors.css
    primitives.css
  semantic/
    themes.css
    contexts.css
  components/
    button.css
    card.css
```

## Dark Mode Implementation

Use semantic tokens for theme switching:

```css
:root {
  /* Foundation tokens - never change */
  --foundation-gray-900: #111827;
  --foundation-gray-100: #f3f4f6;
  --foundation-white: #ffffff;
}

[data-theme="light"] {
  /* Semantic tokens - map to foundation */
  --color-background: var(--foundation-white);
  --color-text: var(--foundation-gray-900);
  --color-surface: var(--foundation-gray-100);
}

[data-theme="dark"] {
  --color-background: var(--foundation-gray-900);
  --color-text: var(--foundation-white);
  --color-surface: var(--foundation-gray-800);
}
```

## Testing Tokens

Verify:
- All tokens used (no unused definitions)
- No magic numbers in codebase
- Token names follow convention
- Semantic tokens reference foundation tokens
- Component tokens reference semantic tokens
- Dark mode uses same semantic token names
