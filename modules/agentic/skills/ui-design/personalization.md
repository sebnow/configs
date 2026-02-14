# AI-Driven Personalization

Ethical patterns for implementing AI-powered interface adaptation.

## Core Requirements

Every AI personalization feature must include:

1. **Transparency:** Users know when AI personalizes their experience
2. **Control:** Opt-out and reset options available
3. **Explanation:** Show why adaptations occurred
4. **Human fallback:** Support when AI fails
5. **Privacy:** No personalization with sensitive data without consent

## Consent Management

Always get explicit consent before personalizing:

```javascript
const personalizationConsent = {
  async request() {
    const consent = await showConsentDialog({
      title: 'Personalize your experience',
      description: 'We can adapt the interface based on your usage patterns to improve your workflow.',
      features: [
        'Layout density preferences',
        'Content prioritization',
        'Navigation shortcuts'
      ],
      privacy: 'Your preferences stay on your device. We never share usage data.',
      buttons: {
        accept: 'Enable personalization',
        decline: 'Use default layout'
      }
    });

    localStorage.setItem('personalization-consent', consent);
    return consent;
  },

  get() {
    return localStorage.getItem('personalization-consent') === 'true';
  },

  revoke() {
    localStorage.setItem('personalization-consent', 'false');
    this.reset();
  }
};
```

## Implementation Pattern

```javascript
class PersonalizedUI {
  constructor() {
    this.enabled = personalizationConsent.get();
    this.preferences = this.loadPreferences();
  }

  loadPreferences() {
    const stored = localStorage.getItem('ui-preferences');
    return stored ? JSON.parse(stored) : this.getDefaults();
  }

  getDefaults() {
    return {
      layoutDensity: 'comfortable',
      sidebarPosition: 'left',
      contentOrder: [],
      shortcuts: []
    };
  }

  async adaptLayout(userBehavior) {
    if (!this.enabled) return;

    // Analyze patterns
    const preferences = await this.analyzePatterns(userBehavior);

    // Apply adaptations
    this.applyLayoutPreferences(preferences);

    // Show notification
    this.showAdaptationNotice(preferences);
  }

  async analyzePatterns(behavior) {
    // Example: Detect layout density preference
    const avgActionsPerSession = behavior.interactions.length / behavior.sessions;

    if (avgActionsPerSession > 50) {
      return { layoutDensity: 'compact' };
    } else if (avgActionsPerSession < 20) {
      return { layoutDensity: 'spacious' };
    }

    return {};
  }

  applyLayoutPreferences(preferences) {
    // Apply changes with animation
    document.body.dataset.layoutDensity = preferences.layoutDensity;

    // Save preferences
    this.preferences = { ...this.preferences, ...preferences };
    localStorage.setItem('ui-preferences', JSON.stringify(this.preferences));
  }

  showAdaptationNotice(preferences) {
    // Show toast notification
    showToast({
      message: `We adjusted your layout to ${preferences.layoutDensity} density based on your usage`,
      action: {
        label: 'Undo',
        onClick: () => this.undoLastAdaptation()
      },
      duration: 8000 // Give time to read and act
    });
  }

  undoLastAdaptation() {
    // Restore previous preferences
    const previous = JSON.parse(localStorage.getItem('ui-preferences-previous'));
    this.applyLayoutPreferences(previous);
  }

  reset() {
    // Reset to defaults
    this.preferences = this.getDefaults();
    localStorage.removeItem('ui-preferences');
    localStorage.removeItem('ui-preferences-previous');

    // Notify user
    showToast({
      message: 'Personalization reset to defaults',
      type: 'success'
    });

    // Re-render with defaults
    this.applyLayoutPreferences(this.preferences);
  }
}
```

## Appropriate Personalization

Safe areas for personalization:

### Layout Density

```javascript
function adaptLayoutDensity(usage) {
  // Power users prefer compact layouts
  if (usage.actionsPerMinute > 5) {
    return 'compact';
  }

  // Casual users prefer spacious layouts
  if (usage.actionsPerMinute < 2) {
    return 'spacious';
  }

  return 'comfortable';
}
```

### Content Prioritization

```javascript
function prioritizeContent(interactions) {
  // Track which content types user engages with
  const contentTypes = interactions.reduce((acc, interaction) => {
    acc[interaction.contentType] = (acc[interaction.contentType] || 0) + 1;
    return acc;
  }, {});

  // Sort by engagement
  return Object.entries(contentTypes)
    .sort(([, a], [, b]) => b - a)
    .map(([type]) => type);
}
```

### Navigation Shortcuts

```javascript
function suggestShortcuts(navigation) {
  // Find frequently accessed paths
  const pathCounts = navigation.reduce((acc, path) => {
    acc[path] = (acc[path] || 0) + 1;
    return acc;
  }, {});

  // Suggest shortcuts for top 5 paths
  return Object.entries(pathCounts)
    .sort(([, a], [, b]) => b - a)
    .slice(0, 5)
    .map(([path, count]) => ({
      path,
      frequency: count,
      suggested: true
    }));
}
```

### Theme Preferences

```javascript
function detectThemePreference(usage) {
  // Detect if user changes theme based on time of day
  const timeOfDay = new Date().getHours();

  if (usage.darkModeChanges.length > 5) {
    // User actively changes theme - respect manual choice
    return 'manual';
  }

  // Auto-switch based on time if user hasn't objected
  if (timeOfDay >= 6 && timeOfDay < 18) {
    return 'light';
  } else {
    return 'dark';
  }
}
```

## Forbidden Personalization

Never personalize these areas:

### Critical Functionality

```javascript
// BAD - Never hide critical features
function hideUnusedFeatures(usage) {
  // This breaks discoverability and can trap users
  if (!usage.exportButton) {
    document.querySelector('.export-button').style.display = 'none';
  }
}
```

### Accessibility Features

```javascript
// BAD - Never remove accessibility features
function optimizeForPowerUsers(usage) {
  // Skip links and ARIA labels are not "clutter"
  if (usage.keyboardNavigation === 0) {
    document.querySelector('.skip-link').remove();
  }
}
```

### Core Workflows

```javascript
// BAD - Never change established workflows
function streamlineCheckout(usage) {
  // Users expect consistent checkout flows
  if (usage.returningCustomer) {
    skipConfirmationStep();
  }
}
```

## Transparency UI

Show personalization status clearly:

```html
<!-- Settings panel -->
<section class="personalization-settings">
  <h2>Personalization</h2>

  <div class="setting">
    <label>
      <input type="checkbox" id="personalization-toggle" />
      Enable personalized layout
    </label>
    <p class="description">
      Adapt interface based on your usage patterns
    </p>
  </div>

  <div class="current-adaptations">
    <h3>Current adaptations</h3>
    <ul>
      <li>
        <span>Compact layout density</span>
        <button type="button" class="undo-button">Undo</button>
      </li>
      <li>
        <span>Sidebar on right side</span>
        <button type="button" class="undo-button">Undo</button>
      </li>
    </ul>
  </div>

  <button type="button" class="reset-button">
    Reset all personalizations
  </button>
</section>
```

## Explanation UI

Explain why adaptations occurred:

```javascript
function showExplanation(adaptation) {
  showTooltip({
    message: `We moved this to the top because you use it ${adaptation.frequency} times per session`,
    action: {
      label: 'Learn more',
      onClick: () => showPersonalizationHelp()
    }
  });
}
```

## Performance Considerations

Track usage without impacting performance:

```javascript
// Debounce tracking
const trackInteraction = debounce((interaction) => {
  const batch = getBatchQueue();
  batch.push(interaction);

  if (batch.length >= 50) {
    processBatch(batch);
  }
}, 1000);

// Process in background
function processBatch(interactions) {
  requestIdleCallback(() => {
    analyzePatterns(interactions);
    updatePersonalization();
  });
}
```

## Testing Personalization

Verify:
- Consent flow is clear and not coercive
- Opt-out persists across sessions
- Reset completely removes adaptations
- Explanations are understandable
- No personalization occurs without consent
- Adaptations improve (not harm) UX
- Edge cases handled gracefully
- Performance impact is minimal
