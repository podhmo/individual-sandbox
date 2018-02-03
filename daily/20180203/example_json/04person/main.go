package main

import (
	"io"
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

// WritePerson :
func WritePerson(p *Person, w io.Writer) {
	io.WriteString(w, `{`)
	io.WriteString(w, `"id": `)
	io.WriteString(w, strconv.FormatInt(int64(p.ID), 10))
	io.WriteString(w, `, "name": "`)
	io.WriteString(w, p.Name)
	io.WriteString(w, `", "birthday": "`)
	io.WriteString(w, p.Birthday)
	io.WriteString(w, `", "vivid_info": `)
	WriteVividInfo(&p.VividInfo, w)
	io.WriteString(w, `}`)
}

// WriteVividInfo :
func WriteVividInfo(v *VividInfo, w io.Writer) {
	io.WriteString(w, `{`)
	io.WriteString(w, `"color": "`)
	io.WriteString(w, v.Color)
	io.WriteString(w, `", "weapon": "`)
	io.WriteString(w, v.Weapon)
	io.WriteString(w, `"}`)
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
