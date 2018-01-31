package main

import (
	"fmt"
)

func doSomething(name string) (string, error) {
	fmt.Println("  do something for", name)
	return "ok", nil
}
func doSomething2(name string) error {
	fmt.Println("  do something for", name)
	return nil
}

func notifySlackForX(status string) {
	fmt.Println("----------------------------------------")
	fmt.Println("slack (#x-notification)")
	fmt.Println("...", status)
	fmt.Println("----------------------------------------")
}

func runX() error {
	result, err := doSomething("X")
	if err != nil {
		return err
	}
	notifySlackForX(result)
	if err := doSomething2("X"); err != nil {
		return err
	}
	return nil
}

func main() {
	runX()
}
