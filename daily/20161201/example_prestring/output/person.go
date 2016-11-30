package autogen

type Person struct {
	Name   string `json:"name"`
	Skills struct {
		Name  string `json:"name"`
		Sugoi string `json:"sugoi,omitempty"`
	} `json:"skills"`
}
