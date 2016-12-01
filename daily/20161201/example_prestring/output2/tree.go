package autogen

/* structure
Tree
	Left
	Right
*/
type Tree struct {
	Left  Tree `json:"left"`
	Right Tree `json:"right"`
	Total int  `json:"total"`
}
