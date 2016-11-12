package def

// ID is id
type ID string

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

	/* group
	 */
	Group *Group `json:"group,omitempty"`
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
