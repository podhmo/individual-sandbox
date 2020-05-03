package main

type Node struct {
	Kind string `json:"kind"`
	Atom *Atom `json:"atom"`
	Composite *Composite `json:"composite"`
}


type Atom struct {
	Type string `json:"type"`
}

type Composite struct {
	Type string `json:"type"`
	Args []Node `json:"args"`
}
