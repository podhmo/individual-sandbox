package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
)

// https://swagger.io/docs/specification/data-models/oneof-anyof-allof-not/

type Dog struct {
	Bark  bool
	Breed string // enum: [Dingo, Husky, Retriever, Shepherd]
}

type Cat struct {
	Hunts bool
	Age   int
}

type Pet struct {
	Type string `json:"$type"`
	*Dog `json:"dog,omitempty"`
	*Cat `json:"cat,omitempty"`
}

func main() {
	type S struct {
		Pet Pet
	}

	{
		s := S{Pet: Pet{Type: "Dog", Dog: &Dog{}}}
		enc := json.NewEncoder(os.Stdout)
		enc.SetIndent("", "  ")
		if err := enc.Encode(s); err != nil {
			panic(err)
		}
	}
	fmt.Println("----------------------------------------")
	{
		code := `{"Pet": {"$type": "Dog", "dog": {"Bark": true, "Breed": "Dingo"}}}`
		dec := json.NewDecoder(bytes.NewBufferString(code))
		var s S
		if err := dec.Decode(&s); err != nil {
			panic(err)
		}
		fmt.Printf("%#+v\n", s)

		enc := json.NewEncoder(os.Stdout)
		enc.SetIndent("", "  ")
		if err := enc.Encode(s); err != nil {
			panic(err)
		}

		switch pet := s.Pet; {
		case pet.Dog != nil: // Dog?
			fmt.Println("d", pet.Dog)
		case pet.Cat != nil: // Cat?
			fmt.Println("c", pet.Cat)
		default:
			fmt.Println("hmm", pet)
		}
	}
}
