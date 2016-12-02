package autogen

/* structure
Person
    Skills
*/
type Person struct {
	Name   string `json:"name" example:"foo"`
	Skills Skills `json:"skills"`
}

type Skills []struct {
	Name  string `json:"name" example:"magik"`
	Sugoi string `json:"sugoi.omitempty" example:"yabai"`
}
