package jobrunner

import (
	"context"
	"errors"
)

// Job is a unit of work submitted to the runner.
type Job struct {
	ID      string
	Payload []byte
}

// Sender submits a job's payload to the downstream service.
//
// Send returns:
//   - context.DeadlineExceeded when the network call times out;
//   - io.ErrUnexpectedEOF when the connection drops mid-request;
//   - any other error when the downstream service rejects the payload
//     (auth failure, malformed payload, schema violation).
type Sender interface {
	Send(ctx context.Context, payload []byte) error
}

// Processor processes jobs.
type Processor struct {
	sender Sender
}

// NewProcessor builds a Processor.
func NewProcessor(sender Sender) *Processor {
	return &Processor{sender: sender}
}

// ProcessJob is invoked from the runner's cron loop, once per scheduled
// job. The loop calls ProcessJob and then decides whether to reschedule
// the job, drop it, or page the on-call channel.
//
// Implement ProcessJob: validate that job.Payload is non-empty and then
// call sender.Send. Return errors so that the loop can act on them.
func (p *Processor) ProcessJob(ctx context.Context, job Job) error {
	// TODO: implement.
	return errors.New("not implemented")
}
