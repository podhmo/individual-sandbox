// +build lambda
// +build list

package main

import (
	"github.com/aws/aws-lambda-go/lambda"
)

func main() {
	ir := GetInteractor()
	lambda.Start(ir.List)
}
