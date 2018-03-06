package main

import (
	"bytes"
	"encoding/gob"
	"fmt"
	"io"
	"log"
	"os"

	"github.com/k0kubun/pp"
)

// S :
type S struct {
	Name string
}

// String :
func (s *S) String() string {
	return s.Name
}

// S2 :
type S2 struct {
	S *S
}

// String :
func (s *S2) String() string {
	return s.S.Name
}

// S3 :
type S3 struct {
	S fmt.Stringer
}

// String :
func (s *S3) String() string {
	return s.S.String()
}

func run(ob fmt.Stringer, ref interface{}) error {
	var b bytes.Buffer
	encoder := gob.NewEncoder(io.MultiWriter(&b, os.Stdout))
	if err := encoder.Encode(ob); err != nil {
		return err
	}
	decoder := gob.NewDecoder(&b)
	if err := decoder.Decode(ref); err != nil {
		return err
	}
	fmt.Println("\n----------------------------------------")
	pp.Println(ref)
	return nil
}
func main() {
	pp.ColoringEnabled = false
	{
		var s S
		if err := run(&S{Name: "foo"}, &s); err != nil {
			fmt.Println("\n----------------------------------------")
			log.Fatal(err)
		}
	}
	{
		var s S2
		if err := run(&S2{S: &S{Name: "foo"}}, &s); err != nil {
			fmt.Println("\n----------------------------------------")
			log.Fatal(err)
		}
	}
	{
		var s S3
		if err := run(&S3{S: &S{Name: "foo"}}, &s); err != nil {
			fmt.Println("\n----------------------------------------")
			log.Fatal(err)
		}
	}
}
