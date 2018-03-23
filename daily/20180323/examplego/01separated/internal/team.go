package internal

import (
	"sync"
)

// Team :
type Team struct {
	Name     string
	Services *[]Service
}

// Update :
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

// GetTeamsChanel :
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
