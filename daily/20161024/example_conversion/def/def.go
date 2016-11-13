package def

// ID is id
type ID string

// Gender is gender
type Gender string

const (
	GenderFemale = Gender("xx")
	GenderMALE   = Gender("xy")
)

// Page is page
type Page struct {
	/* id
	 */
	ID *ID `json:"id,omitempty"`

	/* path
	 */
	Path *string `json:"path,omitempty"`

	/* title
	 */
	Title *string `json:"title,omitempty"`
}

// User is user
type User struct {
	/* id
	 */
	ID *ID `json:"id,omitempty"`

	/* name
	 */
	Name *string `json:"name,omitempty"`

	/* gender
	 */
	Gender *Gender `json:"gender,omitempty"`

	/* group
	 */
	Group *Group `json:"group,omitempty"`

	/* skills
	 */
	Skills []Skill `json:"skills,omitempty"`
	/* skills2
	 */
	Skills2 []Skill `json:"skills2,omitempty"`
	/* skills3
	 */
	Skills3 []*Skill `json:"skills3,omitempty"`
}

// Skill is skill
type Skill struct {
	/* name
	 */
	Name *string `json:"name,omitempty"`
}

// Group is group
type Group struct {
	/* id
	 */
	ID *ID `json:"id,omitempty"`

	/* name
	 */
	Name *string `json:"name,omitempty"`
}
