package main

import "fmt"

func f() {
	fmt.Println("start")
	if true {
        g()
		return
	}
	fmt.Println("hmm")
}

func g() {
}

func main() {

}
