Job processor invoked from a cron-style runner.

The runner schedules jobs and calls `ProcessJob` for each one. After the call
returns, the runner decides whether to reschedule the job, drop it, or page
on-call. The implementation stub in `processor.go` returns a generic
"not implemented" error today.

The task is to implement `ProcessJob`.
