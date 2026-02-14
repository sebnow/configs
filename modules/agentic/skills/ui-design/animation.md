# Animation & Motion

Detailed patterns for implementing purposeful motion in UI design.

## Core Principles

- Prioritize high-impact moments
- Use proper easing curves (never linear)
- Respect prefers-reduced-motion
- Performance-conscious implementation
- Purposeful animations only

## Page Load Orchestration

Staggered reveal for sequential elements:

```css
.item {
  animation: reveal 0.6s cubic-bezier(0.16, 1, 0.3, 1) backwards;
}

.item:nth-child(1) { animation-delay: 0.1s; }
.item:nth-child(2) { animation-delay: 0.2s; }
.item:nth-child(3) { animation-delay: 0.3s; }
.item:nth-child(4) { animation-delay: 0.4s; }

@keyframes reveal {
  from {
    opacity: 0;
    transform: translateY(20px);
  }
  to {
    opacity: 1;
    transform: translateY(0);
  }
}
```

## Scroll-Triggered Effects

Use Intersection Observer for performance:

```javascript
const observer = new IntersectionObserver(
  (entries) => {
    entries.forEach((entry) => {
      if (entry.isIntersecting) {
        entry.target.classList.add('is-visible');
        // Optionally unobserve after animation
        observer.unobserve(entry.target);
      }
    });
  },
  {
    threshold: 0.1,
    rootMargin: '0px 0px -100px 0px' // Trigger before element is fully visible
  }
);

// Observe all animated elements
document.querySelectorAll('.animate-on-scroll').forEach((el) => {
  observer.observe(el);
});
```

```css
.animate-on-scroll {
  opacity: 0;
  transform: translateY(30px);
  transition: opacity 0.6s cubic-bezier(0.16, 1, 0.3, 1),
              transform 0.6s cubic-bezier(0.16, 1, 0.3, 1);
}

.animate-on-scroll.is-visible {
  opacity: 1;
  transform: translateY(0);
}
```

## Easing Curves

Never use linear easing. Recommended curves:

```css
/* Smooth deceleration - most versatile */
--ease-out: cubic-bezier(0.16, 1, 0.3, 1);

/* Dramatic ease - for emphasis */
--ease-dramatic: cubic-bezier(0.87, 0, 0.13, 1);

/* Slight bounce - playful interactions */
--ease-bounce: cubic-bezier(0.34, 1.56, 0.64, 1);

/* Quick snap - immediate feedback */
--ease-snap: cubic-bezier(0.4, 0, 0.2, 1);

/* Smooth ease-in-out - balanced motion */
--ease-smooth: cubic-bezier(0.65, 0, 0.35, 1);
```

Usage example:

```css
.button {
  transition: transform 0.2s var(--ease-out);
}

.button:hover {
  transform: translateY(-2px);
}

.button:active {
  transform: translateY(0);
  transition-duration: 0.1s;
}
```

## Hover States

Subtle but noticeable interactions:

```css
.card {
  transition: transform 0.3s var(--ease-out),
              box-shadow 0.3s var(--ease-out);
}

.card:hover {
  transform: translateY(-4px);
  box-shadow: 0 12px 24px rgba(0, 0, 0, 0.15);
}
```

## Loading States

Skeleton screens with shimmer effect:

```css
.skeleton {
  background: linear-gradient(
    90deg,
    var(--color-gray-200) 0%,
    var(--color-gray-100) 50%,
    var(--color-gray-200) 100%
  );
  background-size: 200% 100%;
  animation: shimmer 1.5s ease-in-out infinite;
}

@keyframes shimmer {
  0% { background-position: 200% 0; }
  100% { background-position: -200% 0; }
}
```

Spinner with smooth rotation:

```css
.spinner {
  animation: rotate 1s linear infinite;
}

@keyframes rotate {
  from { transform: rotate(0deg); }
  to { transform: rotate(360deg); }
}
```

## Micro-interactions

Checkbox animation:

```css
.checkbox input:checked + .checkmark {
  animation: checkmark-pop 0.3s var(--ease-bounce);
}

@keyframes checkmark-pop {
  0% { transform: scale(0); }
  50% { transform: scale(1.2); }
  100% { transform: scale(1); }
}
```

## Parallax Effects

Layered parallax for depth:

```javascript
window.addEventListener('scroll', () => {
  const scrolled = window.pageYOffset;

  document.querySelectorAll('.parallax-layer').forEach((layer) => {
    const speed = layer.dataset.speed || 0.5;
    layer.style.transform = `translateY(${scrolled * speed}px)`;
  });
});
```

Use CSS transforms for better performance:

```css
.parallax-layer {
  will-change: transform;
  transform: translateZ(0); /* Force GPU acceleration */
}
```

## Modal Transitions

Smooth modal appearance:

```css
.modal {
  opacity: 0;
  transform: scale(0.95);
  transition: opacity 0.2s var(--ease-out),
              transform 0.2s var(--ease-out);
}

.modal.is-open {
  opacity: 1;
  transform: scale(1);
}

.modal-backdrop {
  opacity: 0;
  transition: opacity 0.3s ease;
}

.modal-backdrop.is-open {
  opacity: 1;
}
```

## Accessibility: Reduced Motion

Always respect user preferences:

```css
@media (prefers-reduced-motion: reduce) {
  *,
  *::before,
  *::after {
    animation-duration: 0.01ms !important;
    animation-iteration-count: 1 !important;
    transition-duration: 0.01ms !important;
    scroll-behavior: auto !important;
  }
}
```

Provide alternative feedback for reduced motion:

```javascript
const prefersReducedMotion = window.matchMedia('(prefers-reduced-motion: reduce)').matches;

if (prefersReducedMotion) {
  // Use instant state changes instead of animations
  element.classList.add('no-animation');
}
```

## Performance Optimization

Use CSS `will-change` sparingly:

```css
/* Only on elements that will definitely animate */
.button:hover {
  will-change: transform;
}

.button:not(:hover) {
  will-change: auto; /* Remove after animation */
}
```

Prefer transforms and opacity:

```css
/* Good - GPU accelerated */
.element {
  transform: translateX(100px);
  opacity: 0.5;
}

/* Bad - triggers layout/paint */
.element {
  left: 100px;
  background: rgba(0, 0, 0, 0.5);
}
```

## Animation Duration Guidelines

- Micro-interactions: 100-200ms
- UI feedback: 200-300ms
- Element transitions: 300-500ms
- Page transitions: 500-800ms
- Never exceed 1000ms for UI animations

## Testing Animations

Verify across:
- Different frame rates (60fps, 120fps, 30fps)
- Low-end devices
- prefers-reduced-motion enabled
- Touch vs mouse interactions
