package deps

import (
	"../accessing"
)

// Status is x
type Status int

const (
	Waiting Status = iota
	Initialized
	Pending
	Cancelled
	Error
	Finished
)

// State is x
type State struct {
	JobID         accessing.JobID
	Params        map[string]string // xxx
	Status        Status
	EndNode       bool
	StatusAPIPath Endpoint
}
