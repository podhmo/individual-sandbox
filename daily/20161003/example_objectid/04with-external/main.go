package main

import (
	"log"
)

func main() {
	dummyAccessor := DummyAccessor{}
	sw := StateWalker{
		Settings: &Settings,
		gen:      NewOnMemoryObjectIDGenerator(),
		accessor: &dummyAccessor,
	}
	analysis := Analysis{
		targetID: "targetId",
		xID:      "xid",
		yID:      "yid",
		zID:      "zid",
	}
	ws := NewWholeState(&analysis)

	// patten 1
	pattern1 := []string{"taskB0", "taskB4", "taskA3"}
	err := sw.WalkByNames(ws, pattern1)
	if err != nil {
		panic(err)
	}
	log.Printf("%#v\n", ws)
}

// Settings is x
var Settings ServiceSettings

/*
task dependencies
targetId -> taskA0 -> taskB0 (end)
targetId, xId -> task1 -> taskB1 (end)
yId -> taskA2 -> taskB2 (end)
xId, yId, zId -> taskA3 (end)
targetId, xId -> taskA4
targetId, yId -> taskA5
taskA4, taskA5 -> taskB3 (end)
taskA0, taskA1, taskA2 -> taskB4(end)
*/

func init() {
	Settings = ServiceSettings{
		"taskA0": ServiceSpec{
			BasePath:            "/api/task/A0/",
			ServiceDependencies: []string{},
			ItemDependencies:    []string{"targetID"},
			EndNode:             false,
		},
		"taskB0": ServiceSpec{
			BasePath:            "/api/task/B0/",
			ServiceDependencies: []string{"taskA1"},
			ItemDependencies:    []string{},
			EndNode:             true,
		},
		"taskA1": ServiceSpec{
			BasePath:            "/api/task/A1/",
			ServiceDependencies: []string{},
			ItemDependencies:    []string{"targetID", "xID"},
			EndNode:             false,
		},
		"taskB1": ServiceSpec{
			BasePath:            "/api/task/B1/",
			ServiceDependencies: []string{"taskA1"},
			ItemDependencies:    []string{},
			EndNode:             true,
		},
		"taskA2": ServiceSpec{
			BasePath:            "/api/task/A2/",
			ServiceDependencies: []string{},
			ItemDependencies:    []string{"yID"},
			EndNode:             false,
		},
		"taskB2": ServiceSpec{
			BasePath:            "/api/task/B2/",
			ServiceDependencies: []string{"taskA2"},
			ItemDependencies:    []string{},
			EndNode:             true,
		},
		"taskA3": ServiceSpec{
			BasePath:            "/api/task/A3/",
			ServiceDependencies: []string{},
			ItemDependencies:    []string{"xID", "yID", "zID"},
			EndNode:             true,
		},
		"taskA4": ServiceSpec{
			BasePath:            "/api/task/A4/",
			ServiceDependencies: []string{"targetID", "xID"},
			ItemDependencies:    []string{},
			EndNode:             false,
		},
		"taskA5": ServiceSpec{
			BasePath:            "/api/task/A5/",
			ServiceDependencies: []string{},
			ItemDependencies:    []string{"targetID", "yID"},
			EndNode:             false,
		},
		"taskB3": ServiceSpec{
			BasePath:            "/api/task/B3/",
			ServiceDependencies: []string{"taskA4", "taskA5"},
			ItemDependencies:    []string{},
			EndNode:             true,
		},
		"taskB4": ServiceSpec{
			BasePath:            "/api/task/B4/",
			ServiceDependencies: []string{"taskA0", "taskA1", "taskA2"},
			ItemDependencies:    []string{},
			EndNode:             true,
		},
	}
}
