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
	recoder := storegolden.NewJSONRecorder()
	if err := recoder.Record(t, GetData()); err != nil {
		t.Fatalf("record: %s", err)
	}
	var want string
	if err := recoder.Replay(t, &want); err != nil {
		t.Fatalf("replay: %s", err)
	}
	got := GetData()
	if want != got {
		t.Fatalf("want=%q, but got=%q", want, got)
	}
	pp.Println(want, got)
}
