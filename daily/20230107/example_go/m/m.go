package m

type Person struct {
	Name     string `json:"name"`
	Age      int
	Nickname *string

	Father   *Person
	Children []Person
}

type Ordering string
type Ordering2 Ordering

type Group struct {
	Members   []Person
	Ordering  Ordering
	Ordering2 Ordering2
}
