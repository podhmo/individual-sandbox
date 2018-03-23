package internal

import "sync"

// GoRoutineWithChannel :
func GoRoutineWithChannel(teams []Team) []Team {
	resultTeams := make([]Team, len(teams))
	var wg sync.WaitGroup
	for i := 0; i < len(teams); i++ {
		wg.Add(1)
		go func(i int) {
			updateTeamAsync(&teams[i])
			resultTeams[i] = teams[i]
			wg.Done()
		}(i)
	}
	wg.Wait()
	return resultTeams
}

func updateTeamAsync(team *Team) {
	services := team.Services
	resultServices := make([]Service, len(services))
	var wg sync.WaitGroup
	for i := 0; i < len(services); i++ {
		wg.Add(1)
		go func(i int) {
			services[i].Update()
			resultServices[i] = services[i]
			wg.Done()
		}(i)
	}
	wg.Wait()
	team.Services = resultServices
}
