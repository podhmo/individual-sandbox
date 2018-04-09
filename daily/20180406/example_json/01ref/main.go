package main

import (
	"encoding/json"
	"fmt"
	"log"
)

type errRef struct {
	Err error
}

// JSONMarshal :
func (e *errRef) MarshalJSON() ([]byte, error) {
	return []byte(fmt.Sprintf(`"%s"`, e.Err.Error())), nil
}

func main() {
	b, err := json.Marshal(&errRef{Err: fmt.Errorf("%s hai", "foo")})
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println("@", string(b), "@")
}
