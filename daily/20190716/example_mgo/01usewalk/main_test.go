package main

import (
	"m/walk"
	"testing"

	"github.com/stretchr/testify/assert"
	"gopkg.in/mgo.v2/bson"
)

func TestIT(t *testing.T) {
	assert.NoError(t, run(t, bson.M{}))
	assert.NoError(t, run(t, bson.M{"id": "xxx"}))
	assert.Error(t, run(t, bson.M{"ID": "xxx"}))
	assert.Error(t, run(t, bson.M{"xxID": "xxx"}))
	assert.NoError(t, run(t, bson.M{"xxId": "xxx"}))
	assert.Error(t, run(t, bson.M{"xx": bson.M{"ID": "xxx"}}))
	assert.NoError(t, run(t, bson.M{"xxx": bson.M{"$in": []int{1, 2, 3}}}))
}

func run(t *testing.T, m bson.M) error {
	t.Logf("input: %+v\n", m)
	return walk.Walk(m)
}
