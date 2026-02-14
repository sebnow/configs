# Component Patterns

Reusable patterns ensuring consistency and accessibility.

## Navigation

### Primary Navigation
- Visible on all viewports (hamburger acceptable on mobile)
- Indicates current location
- Keyboard navigable with visible focus
- ARIA current="page" for active item

### Breadcrumbs
- Hidden on mobile unless critical
- Use nav with aria-label="Breadcrumb"
- Separator as CSS, not content

## Forms

### Text Inputs
- Label always visible (not just placeholder)
- Error messages below field
- Helper text when needed
- Required fields marked clearly
- Minimum 44px touch target

### Validation
- Inline validation after field blur
- Success indicators for valid entries
- Clear error messages with recovery hints
- Summary at form top for submission errors

## Cards

### Content Cards
- Entire card clickable if single action
- Multiple actions clearly separated
- Image alt text describes content, not decoration
- Consistent spacing using design tokens

## Modals

### Dialog Pattern
- Focus trapped while open
- Escape key closes
- Focus returns to trigger on close
- Backdrop click closes (unless destructive)
- Title always present

## Tables

### Responsive Tables
- Horizontal scroll on mobile
- Sticky headers when scrolling
- Row actions at row end
- Sortable columns clearly indicated

## Loading States

### Skeleton Screens
- Match layout of loaded content
- Animate subtly
- Provide loading message for screen readers

### Progress Indicators
- Determinate when possible
- Clear completion messaging
- Allow cancellation for long operations