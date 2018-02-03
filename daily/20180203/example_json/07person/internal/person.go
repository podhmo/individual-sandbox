package internal

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
