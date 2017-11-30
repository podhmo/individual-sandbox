package main

import "fmt"

func main() {
	err := fmt.Errorf("ho")
	if err != nil {
		fmt.Println("hmm")
	}
}
