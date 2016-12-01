package autogen

type Person struct {
	Name   string `json:"name"`
	Skills Skills `json:"skills"`
}

type Skills []struct {
	Name  string `json:"name"`
	Sugoi string `json:"sugoi,omitempty"`
}
