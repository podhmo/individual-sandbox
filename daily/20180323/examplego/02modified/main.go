package main

import (
	"encoding/json"
	"fmt"
	"io"
	"os"

	"./internal"
)

func runWith(title string, w io.Writer, teams []internal.Team, action func(teams []internal.Team) []internal.Team) {
	fmt.Printf("********************************************** %s", title)
	resultTeams := action(teams)
	encoder := json.NewEncoder(w)
	encoder.SetIndent("", "  ")
	if err := encoder.Encode(resultTeams); err != nil {
		fmt.Println("JSON Marshal error:", err)
	}
}

func main() {
	teams := internal.Setup(internal.Opt{
		TeamSize:    5,
		ServiceSize: 5,
	})
	w := os.Stdout

	// runWith("noop", w, teams, func(teams []internal.Team) []internal.Team { return teams })
	// runWith("Go without Channel", w, teams, internal.GoRoutineWithoutChannel)
	runWith("Go with Channel", w, teams, internal.GoRoutineWithChannel)
	// runWith("Go with Sync exec", w, teams, internal.SyncExecution)
}
