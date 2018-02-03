package main

import (
	"fmt"
	"io"
	"os"
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
	fmt.Fprintf(
		w,
		`{"id": %d, "name": %q, "birthday": %q, "vivid_info": `,
		p.ID,
		p.Name,
		p.Birthday,
	)
	WriteVividInfo(&p.VividInfo, w)
	io.WriteString(w, `}`)
}

// WriteVividInfo :
func WriteVividInfo(v *VividInfo, w io.Writer) {
	fmt.Fprintf(
		w,
		`{"color": %q, "weapon": %q}`,
		v.Color,
		v.Weapon,
	)
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
