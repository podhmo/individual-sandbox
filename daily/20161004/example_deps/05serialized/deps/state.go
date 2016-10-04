package deps

import (
	"../accessing"
	"../status"
)

// State is x
type State struct {
	JobID         accessing.JobID
	Params        map[string]string // xxx
	Status        status.Status
	EndNode       bool
	StatusAPIPath string
}
