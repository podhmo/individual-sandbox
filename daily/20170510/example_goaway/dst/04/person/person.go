package person

// Person : ヒト
type Person struct {
	Name   string  `json:"name"`
	Age    int64   `json:"age"`
	Father *Person `json:"father"`
	Mother *Person `json:"mother"`
	Info   Info    `json:"info"`
	Info2  Info    `json:"info2"`
}
