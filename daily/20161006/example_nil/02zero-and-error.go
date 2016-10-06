package main

import (
	"log"
)

type Ob2 interface {
}

func h() (Ob2, error) {
	return nil, nil
}

func main() {
	{
		x, err := h()
		if err != nil {
			log.Fatal(err)
		}
		log.Println(x)

	}
}
