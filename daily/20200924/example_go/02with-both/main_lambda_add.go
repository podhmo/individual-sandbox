// +build lambda
// +build add

package main

import (
	"github.com/aws/aws-lambda-go/lambda"
)

func main() {
	ir := GetInteractor()
	lambda.Start(ir.Add) // buggy
}
