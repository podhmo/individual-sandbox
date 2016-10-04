package deps

// Sentinel is x
type Sentinel struct {
	endpoints []Endpoint
}

// Endpoint is x
type Endpoint struct {
	Service string
	URL     string
}

// NewSentinel is x
func NewSentinel() *Sentinel {
	endpoints := []Endpoint{}
	sentinel := Sentinel{endpoints: endpoints}
	return &sentinel
}

// NewSentinelFromStates is x
func NewSentinelFromStates(states map[string]State) *Sentinel {
	sentinel := NewSentinel()
	for name, s := range states {
		if s.EndNode {
			endpoint := Endpoint{Service: name, URL: s.StatusAPIPath}
			sentinel.endpoints = append(sentinel.endpoints, endpoint)
		}
	}
	return sentinel
}
