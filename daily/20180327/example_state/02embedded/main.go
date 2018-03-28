package main

import "time"

// Status :
type Status int

// Status :
const (
	Waiting Status = iota
	Runing
	Canceled
	Done
)

// State :
type State struct {
	Status          Status
	CanceledAt      *time.Time
	FinishedAt      *time.Time
	ErrorAt         *time.Time
	LastErrorString *time.Time
}

// Job :
type Job struct {
	Name string
	State
}

// Alternative :
type Alternative struct {
	Name      string
	Condition string
	State
	Skipped bool
}

// NewJob :
func NewJob(fs ...func(*Job)) *Job {
	job := &Job{}
	for _, f := range fs {
		f(job)
	}
	return job
}

func main() {
	job := NewJob(func(job *Job) {
		job.Status = Canceled
	})
}
