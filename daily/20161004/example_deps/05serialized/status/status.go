package status

// Status is x
type Status int

const (
	Waiting Status = iota
	Initialized
	Pending
	Cancelled
	Error
	Done
)
