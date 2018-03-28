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

// Subject :
type Subject struct {
	Name            string
	Status          Status
	RetryAt         *time.Time
	CanceledAt      *time.Time
	FinishedAt      time.Time
	ErrorAt         *time.Time
	LastErrorString *time.Time
}

func main() {
}
