package main

import (
	"strconv"
	"testing"
	"time"
)

func TestDefer(t *testing.T) {
	// t.Run()で囲ったgoroutineより早くdeferが呼ばれてしまう
	// see: https://github.com/golang/go/issues/17791
	defer func() { t.Logf("end") }()

	for i := 0; i < 2; i++ {
		i := i
		t.Run("case"+strconv.Itoa(i), func(t *testing.T) {
			t.Parallel()
			defer t.Logf("end: %d", i)
			t.Logf("start: %d", i)
			time.Sleep(1 * time.Second)
		})
	}
}

func TestCleanup(t *testing.T) {
	t.Cleanup(func() { t.Logf("end") })

	for i := 0; i < 2; i++ {
		i := i
		t.Run("case"+strconv.Itoa(i), func(t *testing.T) {
			t.Parallel()
			t.Cleanup(func() { t.Logf("end: %d", i) })
			t.Logf("start: %d", i)
			time.Sleep(1 * time.Second)
		})
	}
}

