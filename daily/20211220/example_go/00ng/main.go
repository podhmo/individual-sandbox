package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
)

type S struct {
	Name     string
	Stringer fmt.Stringer
}

type F struct{ Name string }

func (f *F) String() string { return "F:" + f.Name }

func main() {
	{
		s := S{Name: "Foo", Stringer: &F{Name: "Foo"}}
		fmt.Println(json.NewEncoder(os.Stdout).Encode(s))
	}
	{
		code := `{"Name": "Foo", "Stringer": {}}`
		var s S
		// json: cannot unmarshal object into Go struct field S.Stringer of type fmt.Stringer
		fmt.Println(json.NewDecoder(bytes.NewBufferString(code)).Decode(&s))
	}
}
