package main

import (
	"bytes"
	"encoding/gob"
	"fmt"
	"io"
	"log"
	"os"
)

// S :
type S struct {
	Name string
}

// S2 :
type S2 struct {
	S *S
}

// String :
func (s *S) String() string {
	return s.Name
}

// String :
func (s *S2) String() string {
	return s.S.Name
}

func run(ob fmt.Stringer, ref interface{}) {
	var b bytes.Buffer
	encoder := gob.NewEncoder(io.MultiWriter(&b, os.Stdout))
	if err := encoder.Encode(ob); err != nil {
		log.Fatal(err)
	}
	decoder := gob.NewDecoder(&b)
	if err := decoder.Decode(ref); err != nil {
		log.Fatal(err)
	}
	fmt.Println("\n----------------------------------------")
	fmt.Println(ref)
}
func main() {
	{
		var s S
		run(&S{Name: "foo"}, &s)
	}
	{
		var s S2
		run(&S2{S: &S{Name: "foo"}}, &s)
	}
}
