package main

import (
	"fmt"

	"github.com/go-openapi/swag"
)

func main() {
    // because of SLA
	fmt.Println(swag.ToGoName("slackName"))
}
