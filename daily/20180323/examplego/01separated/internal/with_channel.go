package internal

func GoRoutineWithChannel(teams *[]Team) []Team {
	teamsChannel := GetTeamsChanel(teams)
	newTeamsChannel := UpdateTeamsAsync(teamsChannel)
	var resultTeams []Team
	for i := 0; i < len(*teams); i++ {
		t := <-newTeamsChannel
		resultTeams = append(resultTeams, t)
	}
	return resultTeams
}
