# Skill Categories

Skills fall into three broad categories.
Each category has distinct techniques that make skills effective.

## Document and Asset Creation

Skills that produce artifacts: style guides, templates, reports, checklists.

Key techniques:

- Define clear output format and structure upfront
- Include quality checklists the agent can verify against
- Provide concrete examples of expected output
- Specify file naming conventions and placement

## Workflow Automation

Skills that orchestrate multi-step processes:
validation gates, sequential workflows, plan-validate-execute patterns.

Key techniques:

- Use numbered step-by-step workflows agents can track
- Include validation gates between steps
- Define explicit entry and exit criteria
- Provide templates for repeatable operations
- Require state announcements at phase transitions

## MCP Enhancement

Skills that coordinate MCP servers,
add domain expertise for tool usage,
or provide error handling for external integrations.

Key techniques:

- Document which MCP servers are involved and their capabilities
- Include error handling for common API failures
- Provide domain-specific context the model lacks
- Coordinate multi-server workflows with clear sequencing
- Specify fallback behavior when servers are unavailable
