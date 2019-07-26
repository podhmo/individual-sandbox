package main

import (
	"testing"
)

func Test(t *testing.T) {
	t.Logf("@ %s", t.Name())
	t.Run("x", func(t *testing.T) {
		t.Logf("@@ %s", t.Name())
	})
}
