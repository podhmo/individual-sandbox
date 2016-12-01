package autogen

/* structure
Article
	Comments
		Content
	Content
*/
type Article struct {
	Author   string    `json:"author"`
	Comments Comments  `json:"comments"`
	Content  Content   `json:"content"`
	Ctime    time.Time `json:"ctime"`
	Title    string    `json:"title"`
}

type Comments []struct {
	Author  string      `json:"author"`
	Content ContentDup1 `json:"content"`
	Ctime   time.Time   `json:"ctime"`
}

type Content struct {
	Abbrev string `json:"abbrev"`
	Body   string `json:"body"`
}

type ContentDup1 struct {
	Body string `json:"body"`
}
