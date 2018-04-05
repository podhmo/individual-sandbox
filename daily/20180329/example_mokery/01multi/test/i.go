package test

import "time"

type I interface {
	F()
	G(x string) (int, error)
	H(x string, y time.Time) (chan int, error)
}
