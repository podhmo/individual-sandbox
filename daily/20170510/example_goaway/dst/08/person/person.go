package person

// People :
type People []Person

// Person :
type Person struct {
	Name   string  `json:"name" bson:"name"`
	Age    int64   `json:"age" bson:"age"`
	Gender Gender  `json:"gender" bson:"gender"`
	Father *Person `json:"father" bson:"father"`
	Mother *Person `json:"mother" bson:"mother"`
}
