package main

import (
	"log"
	"os"
	"os/signal"
	"syscall"
	"time"
)

func main() {
	var i int

	log.Println("start", i)
	i++
	log.Println("inc", i)

	defer func() {
		log.Println("end", i)
	}()
	rollback := func() {
		log.Println("rollback", i)
		i--
	}

	c := make(chan os.Signal)
	signal.Notify(c, syscall.SIGINT)
	go func() {
		<-c
		rollback()
		signal.Stop(c)
	}()

	time.Sleep(2 * time.Second)
	rollback()
}
