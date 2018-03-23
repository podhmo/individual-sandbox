package internal

// Team :
type Team struct {
	Name     string
	Services []Service
}

// Update :
func (c *Team) Update() {
	ch := make(chan Service)

	for _, v := range c.Services {
		go func(service Service) {
			service.Update()
			ch <- service
		}(v)
	}
	var services []Service
	for i := 0; i < len(c.Services); i++ {
		result := <-ch
		services = append(services, result)
	}
	c.Services = services
}
