package main

import (
	"fmt"
	"math/rand"
	"sync"
	"testing"
	"time"
)

// Service :
type Service struct {
	Name  string
	Value int
}

// Update :
func (s *Service) Update() {
	s.Value = s.Value + 1
	time.Sleep(time.Duration(rand.Intn(100)) * time.Millisecond)
}

// Team :
type Team struct {
	Name     string
	Services []Service
}

// Update :
func (t *Team) Update() {
	var wg sync.WaitGroup
	newServices := make([]Service, 0, len(t.Services))
	for i := range t.Services {
		i := i
		wg.Add(1)
		go func() {
			t.Services[i].Update()
			newServices = append(newServices, t.Services[i])
			wg.Done()
		}()
	}
	wg.Wait()
	t.Services = newServices
}

// UpdateWithChannel :
func (t *Team) UpdateWithChannel() <-chan *Service {
	ch := make(chan *Service)
	go func() {
		var wg sync.WaitGroup
		for i := range t.Services {
			i := i
			wg.Add(1)
			go func() {
				t.Services[i].Update()
				ch <- &t.Services[i]
				wg.Done()
			}()
		}
		wg.Wait()
		close(ch)
	}()
	return ch
}

// Opt :
type Opt struct {
	TeamSize    int
	ServiceSize int
}

func setup(opt Opt) []Team {
	var teams []Team
	for i := 0; i < opt.TeamSize; i++ {
		team := Team{
			Name: fmt.Sprintf("T%d", i),
		}
		var services []Service
		for j := 0; j < opt.ServiceSize; j++ {
			service := Service{
				Name: fmt.Sprintf("S%d", j),
			}
			services = append(services, service)
		}
		team.Services = services
		teams = append(teams, team)
	}
	return teams
}

func score(teams ...Team) int {
	n := 0
	for i := range teams {
		services := teams[i].Services
		for j := range services {
			n += services[j].Value
		}
	}
	return n
}

func updateTeamsWithoutChannel(teams []Team) []Team {
	var wg sync.WaitGroup
	newTeams := make([]Team, 0, len(teams))
	for i := range teams {
		i := i
		wg.Add(1)
		go func() {
			teams[i].Update()
			newTeams = append(newTeams, teams[i])
			wg.Done()
		}()
	}
	wg.Wait()
	return newTeams
}

func updateTeamsWithChannel(teams []Team) <-chan *Team {
	ch := make(chan *Team, len(teams))
	go func() {
		var wg sync.WaitGroup
		for i := 0; i < len(teams); i++ {
			i := i
			wg.Add(1)
			go func() {
				var services []Service
				for s := range teams[i].UpdateWithChannel() {
					services = append(services, *s)
				}
				teams[i].Services = services
				ch <- &teams[i]
				wg.Done()
			}()
		}
		wg.Wait()
		close(ch)
	}()
	return ch
}

func main() {
	teams := setup(Opt{
		TeamSize:    20,
		ServiceSize: 5,
	})

	{
		r := testing.Benchmark(func(b *testing.B) {
			updateTeamsWithoutChannel(teams)
		})
		fmt.Printf("Benchmark Repeat: %d Duration: %v Bytes: %d  Memory Allocation: %d Memory Bytes: %d\n", r.N, r.T, r.Bytes, r.MemAllocs, r.MemBytes)

	}

	{
		r := testing.Benchmark(func(b *testing.B) {
			for x := range updateTeamsWithChannel(teams) {
				_ = x
			}
		})
		fmt.Printf("Benchmark Repeat: %d Duration: %v Bytes: %d  Memory Allocation: %d Memory Bytes: %d\n", r.N, r.T, r.Bytes, r.MemAllocs, r.MemBytes)
	}

	fmt.Println("ok")
}
