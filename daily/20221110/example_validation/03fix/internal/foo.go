package internal

import (
	"fmt"

	"github.com/pkg/errors"
)

func Foo() error {
	if err := Bar(); err != nil {
		return errors.Wrap(err, "foo --")
	}
	return nil
}

func Bar() error {
	if err := Boo(); err != nil {
		return errors.Wrapf(err, "on %s%d", "bar", 0)
	}
	return nil
}
func Boo() error {
	return fmt.Errorf("hmm")
}

func Multiples() (bool, error) {
	if err := Bar(); err != nil {
		return false, errors.Wrap(err, "multiples")
	}
	return true, nil
}

func NeverChanges() error {
	if err := Bar(); err != nil {
		return fmt.Errorf("foo -- %w", err)
	}
	return nil
}
