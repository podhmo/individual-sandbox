package main

import (
	"testing"

	"github.com/google/go-cmp/cmp"
)

// Code under test:
type ShipManifest struct {
	Name     string
	Crew     map[string]string
	Androids int
	Stolen   bool
}

func Test(t *testing.T) {
	// AddCrew tries to add the given crewmember to the manifest.
	AddCrew := func(m *ShipManifest, name, title string) {
		if m.Crew == nil {
			m.Crew = make(map[string]string)
		}
		m.Crew[title] = name
	}

	// Test function:
	tests := []struct {
		desc        string
		before      *ShipManifest
		name, title string
		after       *ShipManifest
	}{
		{
			desc:   "add to empty",
			before: &ShipManifest{},
			name:   "Zaphod Beeblebrox",
			title:  "Galactic President",
			after: &ShipManifest{
				Crew: map[string]string{
					"Zaphod Beeblebrox": "Galactic President",
				},
			},
		},
		{
			desc: "add another",
			before: &ShipManifest{
				Crew: map[string]string{
					"Zaphod Beeblebrox": "Galactic President",
				},
			},
			name:  "Trillian",
			title: "Human",
			after: &ShipManifest{
				Crew: map[string]string{
					"Zaphod Beeblebrox": "Galactic President",
					"Trillian":          "Human",
				},
			},
		},
		{
			desc: "overwrite",
			before: &ShipManifest{
				Crew: map[string]string{
					"Zaphod Beeblebrox": "Galactic President",
				},
			},
			name:  "Zaphod Beeblebrox",
			title: "Just this guy, you know?",
			after: &ShipManifest{
				Crew: map[string]string{
					"Zaphod Beeblebrox": "Just this guy, you know?",
				},
			},
		},
	}

	for _, test := range tests {
		AddCrew(test.before, test.name, test.title)
		if diff := cmp.Diff(test.before, test.after); diff != "" {
			t.Errorf("%s: after AddCrew, manifest differs: (-got +want)\n%s", test.desc, diff)
		}
	}
}
