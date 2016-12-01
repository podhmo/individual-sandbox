package autogen

type Article struct {
	Author   string `json:"author"`
	Comments struct {
		Author  string `json:"author"`
		Content struct {
			Body string `json:"body"`
		} `json:"content"`
		Ctime time.Time `json:"ctime"`
	} `json:"comments"`
	Content struct {
		Abbrev string `json:"abbrev"`
		Body   string `json:"body"`
	} `json:"content"`
	Ctime time.Time `json:"ctime"`
	Title string    `json:"title"`
}
