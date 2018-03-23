package internal

import "sync"

// GoRoutineWithChannel :
func GoRoutineWithChannel(teams []Team) []Team {
	teamsChannel := GetTeamsChanel(teams)
	newTeamsChannel := UpdateTeamsAsync(teamsChannel)
	var resultTeams []Team
	for i := 0; i < len(teams); i++ {
		t := <-newTeamsChannel
		resultTeams = append(resultTeams, t)
	}
	return resultTeams
}

// GetTeamsChanel :
func GetTeamsChanel(teams []Team) <-chan Team {
	out := make(chan Team, len(teams))
	go func() {
		for _, n := range teams {
			out <- n
		}
		close(out)
	}()
	return out
}

// UpdateTeamsAsync :
func UpdateTeamsAsync(teams ...<-chan Team) <-chan Team {
	var wg sync.WaitGroup
	out := make(chan Team)
	output := func(ts <-chan Team) {
		for t := range ts {
			t.Update()
			out <- t
		}
		wg.Done()
	}
	wg.Add(len(teams))
	for _, t := range teams {
		go output(t)
	}

	go func() {
		wg.Wait()
		close(out)
	}()
	return out
}
