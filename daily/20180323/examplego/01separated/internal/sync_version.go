package internal

// SyncExecution :
func SyncExecution(teams *[]Team) []Team {
	var newTeams []Team
	for _, v := range *teams {
		v.SyncUpdate()
		newTeams = append(newTeams, v)
	}
	return newTeams
}

// SyncUpdate :
func (c *Team) SyncUpdate() {
	var services []Service
	for _, v := range *c.Services {
		v.Update()
		services = append(services, v)
	}
	c.Services = &services
}
