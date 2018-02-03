package main

import (
	"io"
	"os"
	"strconv"
	"strings"
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
	a := make([]string, 0, 124)
	a = writePerson(p, a)
	io.WriteString(w, strings.Join(a, ""))
}

func writePerson(p *Person, r []string) []string {
	r = append(r, `{`)
	r = append(r, `"id": `)
	r = append(r, strconv.FormatInt(int64(p.ID), 10))
	r = append(r, `, "name": "`)
	r = append(r, p.Name)
	r = append(r, `", "birthday": "`)
	r = append(r, p.Birthday)
	r = append(r, `", "vivid_info": `)
	r = writeVividInfo(&p.VividInfo, r)
	r = append(r, `}`)
	return r
}

func writeVividInfo(v *VividInfo, r []string) []string {
	r = append(r, `{`)
	r = append(r, `"color": "`)
	r = append(r, v.Color)
	r = append(r, `", "weapon": "`)
	r = append(r, v.Weapon)
	r = append(r, `"}`)
	return r
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
