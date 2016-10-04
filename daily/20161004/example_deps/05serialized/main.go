package main

import (
	"./analysis"
	"./deps"
	"log"
)

func main() {
	sw := deps.NewDummyStateWalker(&Settings)
	analysis := analysis.Analysis{
		TargetID: "targetId",
		XID:      "xid",
		YID:      "yid",
		ZID:      "zid",
	}
	ws := deps.NewWholeState(&analysis)

	// patten 1
	pattern1 := []string{"taskB0", "taskB4", "taskA3"}
	err := sw.WalkByNames(ws, pattern1)
	if err != nil {
		panic(err)
	}
	log.Printf("%#v\n", ws)
	log.Println("check sentinel status")

	alldone, err := sw.FetchSentinelStatus(ws)
	if err != nil {
		panic(err)
	}
	log.Printf("alldone=%s %#v\n", alldone, ws)
}

// Settings is x
var Settings deps.ServiceSettings

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
	Settings = deps.ServiceSettings{
		"taskA0": deps.ServiceSpec{
			BasePath:            "/api/task/A0/",
			ServiceDependencies: []string{},
			ItemDependencies:    []string{"TargetID"},
			EndNode:             false,
		},
		"taskB0": deps.ServiceSpec{
			BasePath:            "/api/task/B0/",
			ServiceDependencies: []string{"taskA1"},
			ItemDependencies:    []string{},
			EndNode:             true,
		},
		"taskA1": deps.ServiceSpec{
			BasePath:            "/api/task/A1/",
			ServiceDependencies: []string{},
			ItemDependencies:    []string{"TargetID", "XID"},
			EndNode:             false,
		},
		"taskB1": deps.ServiceSpec{
			BasePath:            "/api/task/B1/",
			ServiceDependencies: []string{"taskA1"},
			ItemDependencies:    []string{},
			EndNode:             true,
		},
		"taskA2": deps.ServiceSpec{
			BasePath:            "/api/task/A2/",
			ServiceDependencies: []string{},
			ItemDependencies:    []string{"YID"},
			EndNode:             false,
		},
		"taskB2": deps.ServiceSpec{
			BasePath:            "/api/task/B2/",
			ServiceDependencies: []string{"taskA2"},
			ItemDependencies:    []string{},
			EndNode:             true,
		},
		"taskA3": deps.ServiceSpec{
			BasePath:            "/api/task/A3/",
			ServiceDependencies: []string{},
			ItemDependencies:    []string{"XID", "YID", "ZID"},
			EndNode:             true,
		},
		"taskA4": deps.ServiceSpec{
			BasePath:            "/api/task/A4/",
			ServiceDependencies: []string{"TargetID", "XID"},
			ItemDependencies:    []string{},
			EndNode:             false,
		},
		"taskA5": deps.ServiceSpec{
			BasePath:            "/api/task/A5/",
			ServiceDependencies: []string{},
			ItemDependencies:    []string{"TargetID", "YID"},
			EndNode:             false,
		},
		"taskB3": deps.ServiceSpec{
			BasePath:            "/api/task/B3/",
			ServiceDependencies: []string{"taskA4", "taskA5"},
			ItemDependencies:    []string{},
			EndNode:             true,
		},
		"taskB4": deps.ServiceSpec{
			BasePath:            "/api/task/B4/",
			ServiceDependencies: []string{"taskA0", "taskA1", "taskA2"},
			ItemDependencies:    []string{},
			EndNode:             true,
		},
	}
}
