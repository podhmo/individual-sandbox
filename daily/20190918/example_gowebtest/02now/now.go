package main

import "time"

var now func() time.Time

func Now() time.Time {
	if now != nil {
		return now()
	}
	return time.Now()
}

func WithNow(fn func() time.Time) func() {
	if now != nil {
		// prevとか保持する必要無いと思う。どうせ壊れている
		panic("heh")
	}
	now = fn
	return func() {
		now = nil
	}
}
