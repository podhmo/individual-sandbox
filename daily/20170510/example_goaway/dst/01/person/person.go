package person

// People :
type People []Person

// Person : ヒト
type Person struct {
	Name string `json:"name"`
	Age  int64  `json:"age"`
}

// Group :
type Group struct {
	Name    string `json:"name"`
	Members People `json:"members"`
}
