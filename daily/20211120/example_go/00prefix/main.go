package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
)

type SiteKey string

func (v SiteKey) MarshalText() ([]byte, error) {
	return []byte("st:" + string(v)), nil
}

func (v *SiteKey) UnMarshalText(b []byte) error {
	*v = SiteKey(b[2:])
	return nil
}

func main() {
	// SQL?

	type S struct {
		SK SiteKey `json:"sk"`
	}

	json.NewEncoder(os.Stdout).Encode(S{SK: SiteKey("1")})
	fmt.Println("----------------------------------------")

	var s S
	json.NewDecoder(bytes.NewBufferString(`{"sk": "st:1"}`)).Decode(&s)
	fmt.Println(s)
}
