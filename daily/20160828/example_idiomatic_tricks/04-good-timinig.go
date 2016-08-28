package main

// http://go-talks.appspot.com/github.com/matryer/present/idiomatic-go-tricks/main.slide#14

import (
	"log"
	"time"
)

func StartTimer(name string) func() {
	t := time.Now()
	log.Println(name, "started")

	return func() {
		d := time.Now().Sub(t)
		log.Println(name, "took", d)
	}
}

func FunkyFunc(n int) {
	stop := StartTimer("FunkyFunc")
	defer stop()

	time.Sleep(time.Duration(n) * time.Second)
}

func main() {
	FunkyFunc(1)
}
