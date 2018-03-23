package internal

// GoRoutineWithoutChannel :
func GoRoutineWithoutChannel(teams []Team) []Team {
	ch := make(chan Team)
	for _, v := range teams {
		go func(team Team) {
			team.Update()
			ch <- team
		}(v)
	}
	var newTeams []Team
	for i := 0; i < len(teams); i++ {
		result := <-ch
		newTeams = append(newTeams, result)
	}
	return newTeams
}
