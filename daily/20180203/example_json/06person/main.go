package main

import (
	"os"
	"strconv"
)

// Person :
type Person struct {
	ID        int       `json:"id"`
	Name      string    `json:"name"`
	Birthday  string    `json:"birthday"`
	VividInfo VividInfo `json:"vivid_info"`
}

// VividInfo :
type VividInfo struct {
	Color  string `json:"color"`
	Weapon string `json:"weapon"`
}

type stringWriter interface {
	WriteString(s string) (n int, err error)
}

// WritePerson :
func WritePerson(p *Person, w stringWriter) {
	w.WriteString(`{`)
	w.WriteString(`"id": `)
	w.WriteString(strconv.FormatInt(int64(p.ID), 10))
	w.WriteString(`, "name": "`)
	w.WriteString(p.Name)
	w.WriteString(`", "birthday": "`)
	w.WriteString(p.Birthday)
	w.WriteString(`", "vivid_info": `)
	WriteVividInfo(&p.VividInfo, w)
	w.WriteString(`}`)
}

// WriteVividInfo :
func WriteVividInfo(v *VividInfo, w stringWriter) {
	w.WriteString(`{`)
	w.WriteString(`"color": "`)
	w.WriteString(v.Color)
	w.WriteString(`", "weapon": "`)
	w.WriteString(v.Weapon)
	w.WriteString(`"}`)
}

func main() {
	p := Person{
		ID:       1,
		Name:     "akane",
		Birthday: "08-16",
		VividInfo: VividInfo{
			Color:  "red",
			Weapon: "Rang",
		},
	}
	WritePerson(&p, os.Stdout)
}
