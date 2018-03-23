package internal

import (
	"sort"
)

// GoRoutineWithoutChannelWithSort :
func GoRoutineWithoutChannelWithSort(teams *[]Team) []Team {
	ch := make(chan Team)
	for _, v := range *teams {
		go func(team Team) {
			team.Update()
			ch <- team
		}(v)
	}
	var newTeams []Team
	for i := 0; i < len(*teams); i++ {
		result := <-ch
		newTeams = append(newTeams, result)
	}

	sort.Slice(newTeams, func(i, j int) bool {
		return (newTeams[i].Name < newTeams[j].Name)
	})
	return newTeams
}
