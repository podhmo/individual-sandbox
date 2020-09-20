package main

import (
	"fmt"
	"io"

	"github.com/k0kubun/pp"
	"golang.org/x/xerrors"
)

func main() {
	err := xerrors.Errorf("xxx: %w", xerrors.Errorf("yyy: %w", io.EOF))
	fmt.Printf("!! %+v\n", err)
	pp.Println(err)
	// &xerrors.wrapError{
	//   msg: "xxx",
	//   err: &xerrors.wrapError{
	//     msg: "yyy",
	//     err: &errors.errorString{
	//       s: "EOF",
	//     },
	//     frame: xerrors.Frame{
	//       frames: [3]uintptr{
	//         0x10cc030,
	//         0x10cde4d,
	//         0x1031e0a,
	//       },
	//     },
	//   },
	//   frame: xerrors.Frame{
	//     frames: [3]uintptr{
	//       0x10cc030,
	//       0x10cdea7,
	//       0x1031e0a,
	//     },
	//   },
	// }
}
