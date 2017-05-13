package person

// People :
type People []Person

// Gender :
type Gender string

const (
	// GenderNotKnown :
	GenderNotKnown = Gender("notKnown")
	// GenderMale :
	GenderMale = Gender("male")
	// GenderFemale :
	GenderFemale = Gender("female")
	// GenderNotApplicable :
	GenderNotApplicable = Gender("notApplicable")
)

// Person :
type Person struct {
	Name   string  `json:"name"`
	Age    int64   `json:"age"`
	Gender Gender  `json:"gender"`
	Father *Person `json:"father"`
	Mother *Person `json:"mother"`
}
