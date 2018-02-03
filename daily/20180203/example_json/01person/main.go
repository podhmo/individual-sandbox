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
	w.Write([]byte(`{`))
	w.Write([]byte(`"id": `))
	w.Write([]byte(fmt.Sprintf(`%d`, p.ID)))
	w.Write([]byte(`, "name": `))
	w.Write([]byte(fmt.Sprintf(`%q`, p.Name)))
	w.Write([]byte(`, "birthday": `))
	w.Write([]byte(fmt.Sprintf(`%q`, p.Birthday)))
	w.Write([]byte(`, "vivid_info": `))
	WriteVividInfo(&p.VividInfo, w)
	w.Write([]byte(`}`))
}

// WriteVividInfo :
func WriteVividInfo(v *VividInfo, w io.Writer) {
	w.Write([]byte(`{`))
	w.Write([]byte(`"color": `))
	w.Write([]byte(fmt.Sprintf(`%q`, v.Color)))
	w.Write([]byte(`, "weapon": `))
	w.Write([]byte(fmt.Sprintf(`%q`, v.Weapon)))
	w.Write([]byte(`}`))
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
