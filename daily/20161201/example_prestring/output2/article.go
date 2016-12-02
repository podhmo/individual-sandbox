package autogen

import (
	"time"
)

/* structure
Article
    Comments
        Content
    Content
*/
type Article struct {
	Author   string    `json:"author" example:"foo"`
	Comments Comments  `json:"comments"`
	Content  Content   `json:"content"`
	Ctime    time.Time `json:"ctime" example:"2000-01-01T00:00:00Z"`
	Title    string    `json:"title" example:"It's a first day"`
}

type Comments []struct {
	Author  string      `json:"author" example:"anonymous"`
	Content ContentDup1 `json:"content"`
	Ctime   time.Time   `json:"ctime" example:"2000-01-01T00:00:00Z"`
}

type Content struct {
	Abbrev string `json:"abbrev" example:"Today! this is ..."`
	Body   string `json:"body" example:"Today! this is .... ....."`
}

type ContentDup1 struct {
	Body string `json:"body" example:"xxx"`
}
