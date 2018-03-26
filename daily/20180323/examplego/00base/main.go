package main

// https://qiita.com/TsuyoshiUshio@github/items/9d927784a15b32f3129b

import (
	"encoding/json"
	"fmt"
	"math/rand"
	"sort"
	"sync"
	"testing"
	"time"
)

// Service :
type Service struct {
	Name  string
	Value int
}

// Team :
type Team struct {
	Name     string
	Services *[]Service
}

func (c *Team) Update() {
	ch := make(chan Service)

	for _, v := range *c.Services {
		go func(service Service) {
			service.Update()
			ch <- service
		}(v)
	}
	var services []Service
	for i := 0; i < len(*c.Services); i++ {
		result := <-ch
		services = append(services, result)
	}
	c.Services = &services
}

func (s *Service) Update() {
	s.Value = s.Value + 1
	time.Sleep(time.Duration(rand.Intn(100)) * time.Millisecond)
}

func GetTeamsChanel(teams *[]Team) <-chan Team {
	out := make(chan Team)
	go func() {
		for _, n := range *teams {
			out <- n
		}
		close(out)
	}()
	return out
}

func UpdateTeamsAsync(teams ...<-chan Team) <-chan Team {
	var wg sync.WaitGroup
	out := make(chan Team)
	output := func(ts <-chan Team) {
		for t := range ts {
			t := t
			wg.Add(1)
			go func() {
				t.Update()
				out <- t
				wg.Done()
			}()
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

func GoRoutineWithoutChannel(teams *[]Team) string {
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

	jsonBytes, err := json.Marshal(newTeams)
	if err != nil {
		fmt.Println("JSON Marshal 2 error:", err)
		return "GoRoutineWithoutChannel Error!"
	}
	return string(jsonBytes)
}

func GoRoutineWithoutChannelWithSort(teams *[]Team) string {
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

	jsonBytes, err := json.Marshal(newTeams)
	if err != nil {
		fmt.Println("JSON Marshal 2 error:", err)
		return "GoRoutineWithoutChannel Error!"
	}
	return string(jsonBytes)
}

func GoRoutineWithChannel(teams *[]Team) string {
	teamsChannel := GetTeamsChanel(teams)
	newTeamsChannel := UpdateTeamsAsync(teamsChannel)
	var resultTeams []Team
	for i := 0; i < len(*teams); i++ {
		t := <-newTeamsChannel
		resultTeams = append(resultTeams, t)
	}
	jsonBytes, err := json.Marshal(resultTeams)
	if err != nil {
		fmt.Println("JSON Marshal 3 error:", err)
		return "GoRoutineWitChannel Error!"
	}
	return string(jsonBytes)
}

func SyncExecution(teams *[]Team) string {
	var newTeams []Team
	for _, v := range *teams {
		v.SyncUpdate()
		newTeams = append(newTeams, v)
	}
	jsonBytes, err := json.Marshal(newTeams)
	if err != nil {
		fmt.Println("JSON Marshal 4 error:", err)
		return "Sync Exec Error!"
	}
	return string(jsonBytes)
}

func (c *Team) SyncUpdate() {
	var services []Service
	for _, v := range *c.Services {
		v.Update()
		services = append(services, v)
	}
	c.Services = &services
}

func main() {
	teams := Setup()
	jsonBytes, err := json.Marshal(*teams)
	if err != nil {
		fmt.Println("JSON Marshal error:", err)
		return
	}
	fmt.Println(string(jsonBytes))

	fmt.Println("********************************************** Go without Channel")
	json := GoRoutineWithoutChannel(teams)
	fmt.Println(json)

	fmt.Println("********************************************** Go without Channel with Sort")
	json = GoRoutineWithoutChannelWithSort(teams)
	fmt.Println(json)

	fmt.Println("********************************************** Go with Channel")
	json = GoRoutineWithChannel(teams)
	fmt.Println(json)
	fmt.Println("********************************************** Go with Sync exec")
	json = SyncExecution(teams)
	fmt.Println(json)
	fmt.Println("********************************************** Go without Channel")
	result := testing.Benchmark(func(b *testing.B) { GoRoutineWithoutChannel(teams) })
	fmt.Printf("Benchmark Repeat: %d Duration: %v Bytes: %d  Memory Allocation: %d Memory Bytes: %d\n", result.N, result.T, result.Bytes, result.MemAllocs, result.MemBytes)
	fmt.Println(result)
	fmt.Println("********************************************** Go without Channel with Sort")
	result = testing.Benchmark(func(b *testing.B) { GoRoutineWithoutChannelWithSort(teams) })
	fmt.Printf("Benchmark Repeat: %d Duration: %v Bytes: %d  Memory Allocation: %d Memory Bytes: %d\n", result.N, result.T, result.Bytes, result.MemAllocs, result.MemBytes)
	fmt.Println(result)
	fmt.Println("********************************************** Go with Channel")
	result = testing.Benchmark(func(b *testing.B) { GoRoutineWithChannel(teams) })
	fmt.Printf("Benchmark Repeat: %d Duration: %v Bytes: %d  Memory Allocation: %d Memory Bytes: %d\n", result.N, result.T, result.Bytes, result.MemAllocs, result.MemBytes)
	fmt.Println(result)
	fmt.Println("********************************************** Go with Sync exec")
	result = testing.Benchmark(func(b *testing.B) { SyncExecution(teams) })
	fmt.Printf("Benchmark Repeat: %d Duration: %v Bytes: %d  Memory Allocation: %d Memory Bytes: %d\n", result.N, result.T, result.Bytes, result.MemAllocs, result.MemBytes)
	fmt.Println(result)
}

func Setup() *[]Team {
	var teams []Team
	for i := 0; i < 20; i++ {
		team := Team{
			Name: "Team " + fmt.Sprint(i),
		}
		var services []Service
		for j := 0; j < 5; j++ {
			service := Service{
				Name: "Servicer " + fmt.Sprint(j),
			}
			services = append(services, service)
		}
		team.Services = &services
		teams = append(teams, team)
	}
	return &teams
}
