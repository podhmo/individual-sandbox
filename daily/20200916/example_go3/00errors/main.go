package main

import (
	"fmt"
	"io"

	"github.com/k0kubun/pp"
)

func main() {
	err := fmt.Errorf("xxx: %w", fmt.Errorf("yyy: %w", io.EOF))
	fmt.Printf("!! %+v", err)
	pp.Println(err)

	// &fmt.wrapError{
	//   msg: "xxx yyy EOF",
	//   err: &fmt.wrapError{
	//     msg: "yyy EOF",
	//     err: &errors.errorString{
	//       s: "EOF",
	//     },
	//   },
	// }
}
