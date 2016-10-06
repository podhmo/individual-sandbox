package main

import (
	"log"
)

type Ob struct {
}

func g() (Ob, error) {
	// error
	// return nil, nil
	return Ob{}, nil
}

func g2() (*Ob, error) {
	return nil, nil
}

func main() {
	{
		x, err := g()
		if err != nil {
			log.Fatal(err)
		}
		log.Println(x)

	}
	{
		x, err := g2()
		if err != nil {
			log.Fatal(err)
		}
		log.Println(x)

	}
}
