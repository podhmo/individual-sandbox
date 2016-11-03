package def

// ID is id
type ID string

// Page is page
type Page struct {
	/* path
	 */
	ID *ID `json:"id,omitempty"`

	/* path
	 */
	Path *string `json:"path,omitempty"`

	/* title
	 */
	Title *string `json:"title,omitempty"`
}
