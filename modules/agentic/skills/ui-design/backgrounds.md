# Backgrounds & Atmosphere

Patterns for creating visual richness through layering, textures, and effects.

## Gradient Meshes

Multi-layered radial gradients for depth:

```css
.background {
  background:
    radial-gradient(at 20% 30%, var(--color-accent-1) 0%, transparent 50%),
    radial-gradient(at 80% 70%, var(--color-accent-2) 0%, transparent 50%),
    radial-gradient(at 50% 50%, var(--color-accent-3) 0%, transparent 70%),
    var(--color-background);
}
```

Animated gradient mesh:

```css
.background-animated {
  background:
    radial-gradient(at 20% 30%, var(--color-accent-1) 0%, transparent 50%),
    radial-gradient(at 80% 70%, var(--color-accent-2) 0%, transparent 50%);
  background-size: 200% 200%;
  animation: gradient-shift 15s ease infinite;
}

@keyframes gradient-shift {
  0%, 100% { background-position: 0% 50%; }
  50% { background-position: 100% 50%; }
}
```

## Glass Morphism

Modern frosted glass effect:

```css
.glass {
  background: rgba(255, 255, 255, 0.1);
  backdrop-filter: blur(10px) saturate(180%);
  -webkit-backdrop-filter: blur(10px) saturate(180%);
  border: 1px solid rgba(255, 255, 255, 0.2);
  box-shadow: 0 8px 32px rgba(0, 0, 0, 0.1);
}

/* Dark mode variant */
[data-theme="dark"] .glass {
  background: rgba(0, 0, 0, 0.2);
  border: 1px solid rgba(255, 255, 255, 0.1);
}
```

## Textures & Patterns

Subtle noise for depth:

```css
.surface-textured {
  background-image: url('data:image/svg+xml,<svg xmlns="http://www.w3.org/2000/svg" width="200" height="200"><filter id="noise"><feTurbulence type="fractalNoise" baseFrequency="0.65" numOctaves="3" stitchTiles="stitch"/></filter><rect width="100%" height="100%" filter="url(%23noise)" opacity="0.05"/></svg>');
  background-repeat: repeat;
}
```

Grid pattern:

```css
.grid-background {
  background-image:
    linear-gradient(var(--color-border) 1px, transparent 1px),
    linear-gradient(90deg, var(--color-border) 1px, transparent 1px);
  background-size: 20px 20px;
  background-position: -1px -1px;
}
```

Dots pattern:

```css
.dots-background {
  background-image: radial-gradient(
    circle,
    var(--color-border) 1px,
    transparent 1px
  );
  background-size: 20px 20px;
}
```

## Depth & Layering

Create depth with multiple layers:

```css
.layered-surface {
  position: relative;
}

.layered-surface::before {
  content: '';
  position: absolute;
  inset: 0;
  background: linear-gradient(
    135deg,
    transparent 0%,
    rgba(0, 0, 0, 0.05) 100%
  );
  pointer-events: none;
}

.layered-surface::after {
  content: '';
  position: absolute;
  inset: 0;
  background: radial-gradient(
    circle at top right,
    rgba(255, 255, 255, 0.1) 0%,
    transparent 50%
  );
  pointer-events: none;
}
```

## Light Effects

Inner glow:

```css
.glow-inner {
  box-shadow: inset 0 0 20px rgba(255, 255, 255, 0.1);
}
```

Outer glow:

```css
.glow-outer {
  box-shadow: 0 0 40px rgba(var(--color-primary-rgb), 0.3);
}
```

Spotlight effect:

```css
.spotlight {
  background: radial-gradient(
    ellipse 800px 600px at 50% 0%,
    rgba(var(--color-accent-rgb), 0.15),
    transparent
  );
}
```

## Mesh Gradients

Complex multi-point gradients:

```css
.mesh-gradient {
  background:
    radial-gradient(at 0% 0%, #ff6b6b 0%, transparent 50%),
    radial-gradient(at 100% 0%, #4ecdc4 0%, transparent 50%),
    radial-gradient(at 100% 100%, #ffe66d 0%, transparent 50%),
    radial-gradient(at 0% 100%, #a8e6cf 0%, transparent 50%),
    #f7f7f7;
}
```

## Performance Considerations

Use CSS instead of images when possible:

```css
/* Good - CSS gradient */
background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);

/* Avoid - image file unless necessary */
background: url('/gradients/purple.jpg');
```

Optimize backdrop-filter usage:

```css
/* Use sparingly - expensive operation */
.glass {
  /* Only apply on hover if not critical */
  backdrop-filter: none;
}

.glass:hover,
.glass:focus-within {
  backdrop-filter: blur(10px);
}
```

## Dark Mode Variants

Adjust background intensity for theme:

```css
[data-theme="light"] .background-gradient {
  background: linear-gradient(
    135deg,
    rgba(99, 102, 241, 0.1) 0%,
    rgba(139, 92, 246, 0.1) 100%
  );
}

[data-theme="dark"] .background-gradient {
  background: linear-gradient(
    135deg,
    rgba(99, 102, 241, 0.2) 0%,
    rgba(139, 92, 246, 0.2) 100%
  );
}
```

## Testing Backgrounds

Verify:
- Performance on low-end devices
- Dark mode appearance
- Text readability over backgrounds
- Color contrast ratios
- Browser compatibility (especially backdrop-filter)
