Small Go package exposing the `Invoices` port consumed by billing use cases.

The fixture declares the `Invoices` interface and a domain `Invoice` value
type. The current interface exposes one method, `FindByID`. A new use case
needs the ability to record a payment against an invoice; the task is to add
a single new method to the interface so downstream adapters can implement it.
