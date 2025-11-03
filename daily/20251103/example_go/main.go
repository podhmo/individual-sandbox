package main

import (
	"encoding/json/jsontext"
	json "encoding/json/v2"
	"fmt"
	"os"
	"runtime"
)

func main() {
	if err := run(); err != nil {
		fmt.Fprintln(os.Stderr, "\t!!", err)
	}
}

func run() error {
	fmt.Fprintln(os.Stderr, "go version", runtime.Version())

	jsonString := `{"name": "John Doe", "age": 30, "father": {"name": "Dohn Joe", "age": 60}}`

	{
		fmt.Fprintln(os.Stderr, "Unmarshaling without RejectUnknownMembers")
		var ob Person
		if err := json.Unmarshal([]byte(jsonString), &ob); err != nil {
			return err
		}
		fmt.Printf("\tdecoded %[1]T, %+[1]v\n", ob)
	}

	{
		fmt.Fprintln(os.Stderr, "Unmarshaling with unknown tag options")
		var ob PersonWithExtra
		if err := json.Unmarshal([]byte(jsonString), &ob); err != nil {
			return err
		}
		fmt.Printf("\tdecoded %[1]T, %+[1]v\n", ob)
	}

	{
		fmt.Fprintln(os.Stderr, "Unmarshaling with RejectUnknownMembers")
		var ob Person
		if err := json.Unmarshal([]byte(jsonString), &ob, json.RejectUnknownMembers(true)); err != nil {
			return err
		}
		fmt.Printf("\tdecoded %[1]T, %+[1]v\n", ob)
	}
	return nil
}

type Person struct {
	Name string `json:"name"`
	Age  int    `json:"age"`
}

type PersonWithExtra struct {
	Data  Person         `json:",inline"`
	Extra jsontext.Value `json:",unknown"`
}
