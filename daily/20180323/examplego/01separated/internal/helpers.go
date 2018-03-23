package internal

import "fmt"

// Opt :
type Opt struct {
	TeamSize    int
	ServiceSize int
}

// Setup :
func Setup(opt Opt) []Team {
	var teams []Team
	for i := 0; i < opt.TeamSize; i++ {
		team := Team{
			Name: fmt.Sprintf("Team %d", i),
		}
		var services []Service
		for j := 0; j < opt.ServiceSize; j++ {
			service := Service{
				Name: fmt.Sprintf("Servicer %d", j),
			}
			services = append(services, service)
		}
		team.Services = &services
		teams = append(teams, team)
	}
	return teams
}

func Score(teams []Team) int {
	n := 0
	for i := range teams {
		services := *teams[i].Services
		for j := range services {
			n += services[j].Value
		}
	}
	return n
}
