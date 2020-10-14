package main

import (
	reflectopenapi "github.com/podhmo/reflect-openapi"
)

// FileLinkList is a list of file links as retrieved from a list endpoint.
type FileLinkList struct {
	Data []*FileLink `json:"data"`
}

type File struct {
	Created  int64         `json:"created"`
	ID       string        `json:"id"`
	Filename string        `json:"filename"`
	Links    *FileLinkList `json:"links"`
	Size     int64         `json:"size"`
	Type     string        `json:"type"`
	URL      string        `json:"url"`
}
type FileLink struct {
	Created   int64             `json:"created"`
	Expired   bool              `json:"expired"`
	ExpiresAt int64             `json:"expires_at"`
	File      *File             `json:"file"`
	ID        string            `json:"id"`
	Livemode  bool              `json:"livemode"`
	Metadata  map[string]string `json:"metadata"`
	Object    string            `json:"object"`
	URL       string            `json:"url"`
}

func AccessFile(*File) {
}

func main() {
	var c reflectopenapi.Config
	c.EmitDoc(func(m *reflectopenapi.Manager) {
		op := m.Visitor.VisitFunc(AccessFile)
		m.Doc.AddOperation("/ping", "GET", op)
	})
}
