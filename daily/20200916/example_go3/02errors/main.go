package main

import (
	"fmt"
	"io"

	"github.com/pkg/errors"
)

func F() error {
	err := G()
	return fmt.Errorf("F : %w", err)
}

func G() error {
	err := H()
	return errors.Wrap(err, "G :")
}

func H() error {
	return io.EOF
}

func main() {
	// NOTE: errors.Wrap()で取り出したtracebackの情報が表示されなくなるので注意
	fmt.Printf("!! %+v\n", F())
	// !! F : G :: EOF
}
