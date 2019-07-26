package main

import (
	"m/storegolden"
	"testing"

	"github.com/k0kubun/pp"
)

// GetData :
func GetData() string {
	return "Hello"
}

func Test(t *testing.T) {
	got := GetData()
	storegolden.NewJSONRecorder().RecordAndReplay(t, got, func(got, want interface{}) {
		pp.Println(want, got)
	})
}
