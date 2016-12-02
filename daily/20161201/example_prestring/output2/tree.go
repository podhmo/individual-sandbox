package autogen

/* structure
Tree
    Left
    Right
*/
type Tree struct {
	Left  Left `json:"left"`
	Right Left `json:"right"`
	Total int  `json:"total"`
}
