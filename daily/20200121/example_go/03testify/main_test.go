package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestIt(t *testing.T) {
	val := []int{}
	if assert.NotEmpty(t, val) {
		assert.Exactly(t, val[0], 1)
	}
}

/*
--- FAIL: TestIt (0.00s)
    main_test.go:11: 
        	Error Trace:	main_test.go:11
        	Error:      	Should NOT be empty, but was []
        	Test:       	TestIt
FAIL
*/
