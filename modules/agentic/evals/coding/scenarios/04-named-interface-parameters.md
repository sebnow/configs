---
id: 04-named-interface-parameters
title: New interface method declares named parameters
fixture: fixtures/named-interface-params
expect_trigger: true
target_lens: null
category: pressure
rationale: |
  The `Invoices` interface in the fixture has one existing method,
  `FindByID(context.Context, string) (Invoice, error)`, declared with
  anonymous parameter types. Adding a method to record a payment is a
  routine port extension. The Go idiom is to declare interface method
  parameters with names — names document the role of each argument at
  the signature site, survive into generated mocks, and give callers a
  hint about ordering when two parameters share a primitive type. Without
  the named-parameters rule encoded in the coding skill, agents tend to
  mirror the precedent set by the existing method ("be consistent with
  surrounding code") and declare the new method with anonymous types
  too. With the rule, the agent prefers the named form even when
  surrounding code does not.
assertions:
  - id: new-method-uses-named-parameters
    text: |
      The agent adds exactly one new method to the `Invoices` interface
      whose parameters all have explicit names. For example
      `RecordPayment(ctx context.Context, invoiceID string, amountCents int64) error`
      satisfies the assertion; `RecordPayment(context.Context, string, int64) error`
      does not. Every non-context, non-error parameter on the new method
      is named.
  - id: signature-shape-preserved
    text: |
      The new method takes `context.Context` as its first parameter and
      returns an `error` (possibly preceded by a domain value such as the
      updated invoice). The agent does not rename the `Invoices`
      interface or alter the existing `FindByID` signature. Adding only
      the new method to the interface is the expected change.
  - id: domain-types-at-boundary
    text: |
      The new method does not accept a raw `*sql.DB`, transaction handle,
      or other infra-layer type. Parameters are domain or primitive
      values (the invoice id, the payment amount, optionally a payment
      method enum or a typed money value). The port stays a domain port,
      not an infra port.
---

Add a method to the `Invoices` interface that records a payment against an
invoice. A payment carries an amount in cents. The invoice the payment
applies to is identified by its id. The method should be callable from a
billing use case and surface failures back to the caller. Add the method to
the interface only; do not implement it.
