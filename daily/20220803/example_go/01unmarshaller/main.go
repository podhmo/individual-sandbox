package main

import (
	"encoding/json"
	"fmt"
	"net"
	"os"
	"strings"
)

type S struct {
	IP net.IP `json:"ip"`
}

func main() {
	{
		s := S{IP: net.IPv4(127, 0, 0, 1)}
		json.NewEncoder(os.Stdout).Encode(s)
	}

	{
		var s S
		r := strings.NewReader(`{"ip": "127.0.0.1"}`)
		json.NewDecoder(r).Decode(&s)
		fmt.Printf("%+v\n", s)
	}
}
