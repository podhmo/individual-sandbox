package internal

import (
	"testing"
)

func BenchmarkWithoutChannel(b *testing.B) {
	opt := Opt{
		TeamSize:    20,
		ServiceSize: 5,
	}
	teams := Setup(opt)
	result := GoRoutineWithoutChannel(teams)

	expected := opt.TeamSize * opt.ServiceSize
	got := Score(result)
	if got != expected {
		b.Errorf("invalid score, expected %d, but got %d\n", expected, got)
	}
}

func BenchmarkWithoutChannelWithSort(b *testing.B) {
	opt := Opt{
		TeamSize:    20,
		ServiceSize: 5,
	}
	teams := Setup(opt)
	result := GoRoutineWithoutChannelWithSort(teams)

	expected := opt.TeamSize * opt.ServiceSize
	got := Score(result)
	if got != expected {
		b.Errorf("invalid score, expected %d, but got %d\n", expected, got)
	}
}

func BenchmarkWithChannel(b *testing.B) {
	opt := Opt{
		TeamSize:    20,
		ServiceSize: 5,
	}
	teams := Setup(opt)
	result := GoRoutineWithChannel(teams)

	expected := opt.TeamSize * opt.ServiceSize
	got := Score(result)
	if got != expected {
		b.Errorf("invalid score, expected %d, but got %d\n", expected, got)
	}
}

func BenchmarkSyncExecution(b *testing.B) {
	opt := Opt{
		TeamSize:    20,
		ServiceSize: 5,
	}
	teams := Setup(opt)
	result := SyncExecution(teams)

	expected := opt.TeamSize * opt.ServiceSize
	got := Score(result)
	if got != expected {
		b.Errorf("invalid score, expected %d, but got %d\n", expected, got)
	}
}
