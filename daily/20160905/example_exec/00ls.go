package main

// sync call external command

import (
	"log"
	"os/exec"
)

func main() {
	out, err := exec.Command("ls", "-l").Output()
	if err != nil {
		log.Fatal(err)
	}
	log.Println(string(out))
}
