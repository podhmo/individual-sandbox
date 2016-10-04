package main

// ServiceSpec is x
type ServiceSpec struct {
	BasePath            string
	ServiceDependencies []string
	ItemDependencies    []string
	EndNode             bool
}

// ServiceSettings is x
type ServiceSettings map[string]ServiceSpec
