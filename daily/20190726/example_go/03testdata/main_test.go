package main

import (
	"m/replay"
	"testing"
)

// GetData :
func GetData() string {
	return "Heloo"
}

func Test(t *testing.T) {
	replayCheck := replay.ReplayWith(replay.WithUpdateByEnvvar("X"))

	got := GetData()
	var want string
	replayCheck(t, got, &want, func() {
		if want != got {
			t.Errorf("want %v, but %v", want, got)
		}
	})
}
