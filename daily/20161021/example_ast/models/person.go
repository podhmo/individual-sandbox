package models

// PersonGender : gender
type PersonGender string

// PersonGender : constants
const (
	PersonGenderFemale = PersonGender("female")
	PersonGendermale   = PersonGender("male")
)
const PersonGenderUnknown = PersonGender("unknown")

// Person : person model
type Person struct {
	Name   string       `json:"name"`
	Age    int          `json:"age"`
	Gender PersonGender `json:"gender"`
}
