# Typography Implementation

Detailed patterns for implementing distinctive typography in UI design.

## Basic Implementation

```css
/* Never use generic defaults */
.heading {
  font-family: var(--font-display);
  font-weight: var(--font-weight-bold);
  font-size: var(--text-3xl);
  line-height: var(--leading-tight);
  letter-spacing: var(--tracking-tight);
}

.body {
  font-family: var(--font-body);
  font-weight: var(--font-weight-normal);
  font-size: var(--text-base);
  line-height: var(--leading-relaxed);
}
```

## Variable Fonts

Use variable fonts for fluid weight control:

```css
@supports (font-variation-settings: normal) {
  .heading {
    font-variation-settings: 'wght' 700, 'wdth' 100;
  }

  /* Animate weight on hover */
  .interactive-heading:hover {
    font-variation-settings: 'wght' 800, 'wdth' 100;
    transition: font-variation-settings 0.3s ease;
  }
}
```

## Font Loading Strategy

### Preload Critical Fonts

```html
<link rel="preload" href="/fonts/display.woff2" as="font" type="font/woff2" crossorigin>
<link rel="preload" href="/fonts/body.woff2" as="font" type="font/woff2" crossorigin>
```

### Font Display Strategy

```css
@font-face {
  font-family: 'Display';
  src: url('/fonts/display.woff2') format('woff2');
  font-display: swap; /* Prevent invisible text */
  font-weight: 700 900;
}

@font-face {
  font-family: 'Body';
  src: url('/fonts/body.woff2') format('woff2');
  font-display: swap;
  font-weight: 300 600;
}
```

### Subsetting

Reduce font file size by including only needed characters:

```bash
# Using pyftsubset
pyftsubset font.ttf \
  --unicodes="U+0000-00FF,U+0131,U+0152-0153,U+02BB-02BC,U+02C6,U+02DA,U+02DC,U+2000-206F,U+2074,U+20AC,U+2122,U+2191,U+2193,U+2212,U+2215,U+FEFF,U+FFFD" \
  --output-file="font-subset.woff2" \
  --flavor=woff2
```

## Typographic Scale

Use consistent ratios for visual hierarchy:

```css
/* 1.25 ratio (Major Third) */
--text-xs: 0.64rem;    /* 10.24px */
--text-sm: 0.8rem;     /* 12.8px */
--text-base: 1rem;     /* 16px */
--text-lg: 1.25rem;    /* 20px */
--text-xl: 1.563rem;   /* 25px */
--text-2xl: 1.953rem;  /* 31.25px */
--text-3xl: 2.441rem;  /* 39.06px */
--text-4xl: 3.052rem;  /* 48.83px */

/* 1.414 ratio (Augmented Fourth) for more dramatic contrast */
--text-base: 1rem;
--text-lg: 1.414rem;
--text-xl: 2rem;
--text-2xl: 2.827rem;
--text-3xl: 4rem;
```

## Line Height & Letter Spacing

Adjust based on context:

```css
/* Tight for headings */
--leading-tight: 1.1;
--tracking-tight: -0.02em;

/* Normal for body */
--leading-normal: 1.5;
--tracking-normal: 0;

/* Relaxed for long-form */
--leading-relaxed: 1.75;
--tracking-relaxed: 0.01em;

/* Loose for all-caps */
--leading-loose: 2;
--tracking-loose: 0.1em;
```

## Responsive Typography

Use fluid typography for smooth scaling:

```css
/* Clamp for fluid sizing */
h1 {
  font-size: clamp(2rem, 5vw + 1rem, 4rem);
}

/* Container queries for component-based scaling */
@container (min-width: 400px) {
  .card h2 {
    font-size: var(--text-xl);
  }
}
```

## Readability Optimization

```css
.long-form-content {
  /* Optimal line length: 45-75 characters */
  max-width: 65ch;

  /* Improve readability */
  text-rendering: optimizeLegibility;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;

  /* Prevent orphans */
  text-wrap: pretty;
}
```

## Font Pairing Examples

### Classic Serif + Sans

```css
--font-display: 'Playfair Display', Georgia, serif;
--font-body: 'Source Sans Pro', system-ui, sans-serif;
```

### Modern Geometric

```css
--font-display: 'Poppins', system-ui, sans-serif;
--font-body: 'Inter', system-ui, sans-serif;
```

### Editorial

```css
--font-display: 'Freight Display', Georgia, serif;
--font-body: 'Freight Text', Georgia, serif;
```

### Brutalist

```css
--font-display: 'Space Grotesk', monospace;
--font-body: 'IBM Plex Mono', monospace;
```

## Performance Budget

Target metrics:
- Font load time: <100ms
- Total font weight: <100KB for critical fonts
- Use WOFF2 format exclusively
- Subset fonts to include only used glyphs
