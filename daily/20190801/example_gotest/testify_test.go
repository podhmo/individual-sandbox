package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestTestify(t *testing.T) {
	assert.Exactly(t, 10+10, 20)
	assert.Exactly(t, 10, 20)
}
