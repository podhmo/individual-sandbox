package main

import (
	"strconv"
	"testing"
	"time"
)

func TestDefer(t *testing.T) {
	defer func() { t.Logf("end") }()
	for i := 0; i < 2; i++ {
		i := i
		t.Run("case"+strconv.Itoa(i), func(t *testing.T) {
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
			t.Cleanup(func() { t.Logf("end: %d", i) })
			t.Logf("start: %d", i)
			time.Sleep(1 * time.Second)
		})
	}
}

// === RUN   TestDefer
// === RUN   TestDefer/case0
//     main_test.go:14: start: 0
//     main_test.go:15: end: 0
// === RUN   TestDefer/case1
//     main_test.go:14: start: 1
//     main_test.go:15: end: 1
// === RUN   TestDefer/case2
//     main_test.go:14: start: 2
//     main_test.go:15: end: 2
// === CONT  TestDefer
//     main_test.go:9: end
// --- PASS: TestDefer (0.00s)
//     --- PASS: TestDefer/case0 (0.00s)
//     --- PASS: TestDefer/case1 (0.00s)
//     --- PASS: TestDefer/case2 (0.00s)
//
// === RUN   TestCleanup
// === RUN   TestCleanup/case0
//     main_test.go:25: start: 0
//     main_test.go:24: end: 0
// === RUN   TestCleanup/case1
//     main_test.go:25: start: 1
//     main_test.go:24: end: 1
// === RUN   TestCleanup/case2
//     main_test.go:25: start: 2
//     main_test.go:24: end: 2
// === CONT  TestCleanup
//     main_test.go:20: end
// --- PASS: TestCleanup (0.00s)
//     --- PASS: TestCleanup/case0 (0.00s)
//     --- PASS: TestCleanup/case1 (0.00s)
//     --- PASS: TestCleanup/case2 (0.00s)
