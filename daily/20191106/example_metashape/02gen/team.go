package gen

// Team ...
type Team struct {
	Name    string     `json:"name"`
	Members [][]Person `json:"members"`
}
