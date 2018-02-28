package person

// Person :
type Person struct {
	Name   string  `json:"name"`
	Age    int     `json:"age"`
	Father *Person `json:"father"`
	Mother *Person `json:"mother"`
}
