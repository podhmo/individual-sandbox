package database

import gorp "github.com/go-gorp/gorp/v3"

type Post struct {
	// db tag lets you specify the column name if it differs from the struct field
	Id      int64 `db:"post_id"`
	Created int64
	Title   string `db:",size:50"`               // Column size set to 50
	Body    string `db:"article_body,size:1024"` // Set both column name and size
}

var DefaultRegistry = Registry{
	TableMap: map[string]TableInfo{},
}

func init() {
	DefaultRegistry.TableMap["posts"] = TableInfo{
		Instance: Post{},
		Callback: func(m *gorp.TableMap) {
			m.SetKeys(true, "Id")
		},
	}
}
