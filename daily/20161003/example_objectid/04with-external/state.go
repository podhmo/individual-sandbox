package main

// Status is x
type Status int

// JobID is x
type JobID ObjectID

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
	JobID         JobID
	Params        map[string]string // xxx
	Status        Status
	EndNode       bool
	StatusAPIPath Endpoint
}
