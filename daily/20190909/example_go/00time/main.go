package main

import (
	"fmt"
	"time"
)

type Date string // xxx:

type NowFunc func() time.Time

func (f NowFunc) Now() time.Time {
	return f()
}

func (f NowFunc) Today() Date {
	return Date(f().Format("2006/01/02"))
}

type Clock interface {
	Now() time.Time
	Today() Date
}

func use(clock Clock) {
	fmt.Println("use", clock.Today())
}

func main() {
	use(NowFunc(func() time.Time { return time.Now() }))

}
