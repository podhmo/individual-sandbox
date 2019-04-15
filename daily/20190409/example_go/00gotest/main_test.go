package main

import (
	"fmt"
	"testing"
)

func TestXXX(t *testing.T) {
	t.Run("x", func(t *testing.T) {
		fmt.Println("0")
	})
	t.Run("x", func(t *testing.T) {
		fmt.Println("1")
	})
}
