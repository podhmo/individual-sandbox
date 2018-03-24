package main

import (
	"log"

	"golang.org/x/net/icmp"
)

func main() {
	if err := run(); err != nil {
		log.Fatal(err)
	}
}

func run() error {
	c, err := icmp.ListenPacket("ip4:icmp", "8.8.8.8")
	if err != nil {
        return err
	}
    defer c.Close()
    
}
