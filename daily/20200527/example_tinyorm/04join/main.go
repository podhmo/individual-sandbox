package main

import (
	"database/sql"
	"log"
	"os"
	"time"

	"github.com/go-gorp/gorp/v3"
	_ "github.com/mattn/go-sqlite3"
	"github.com/pkg/errors"
)

// from https://github.com/go-gorp/gorp

type Post struct {
	// db tag lets you specify the column name if it differs from the struct field
	PostID    int64  `db:"postId,primarykey,autoincrement"`
	Published int64  `db:"published"`
	Title     string `db:"title,size:140"` // Column size set to 140
	Url       string `db:"url,size:1024"`  // Set both column name and size
}

type Comment struct {
	CommentID int64  `db:"commentId,primarykey,autoincrement"`
	PostID    int64  `db:"postId"`
	Content   string `db:"content,size:1024"`
}

func initDb() (*gorp.DbMap, error) {
	// connect to db using standard Go database/sql API
	// use whatever database/sql driver you wish
	db, err := sql.Open("sqlite3", ":memory:")
	if err != nil {
		return nil, errors.Wrap(err, "sql.Open failed")

	}

	dbmap := &gorp.DbMap{Db: db, Dialect: gorp.SqliteDialect{}}

	dbmap.AddTableWithName(Post{}, "Post").SetKeys(true, "postId")
	dbmap.AddTableWithName(Comment{}, "Comment").SetKeys(true, "commentId")

	err = dbmap.CreateTablesIfNotExists()
	if err != nil {
		return nil, errors.Wrap(err, "Create tables failed")
	}
	return dbmap, nil
}

func run() error {
	log.SetFlags(0)
	dbmap, err := initDb()
	if err != nil {
		return err
	}
	defer dbmap.Db.Close()

	// setup trace
	dbmap.TraceOn("*trace*", log.New(os.Stdout, "\t", 0))

	p1 := newPost("The Little Go Post", "http://openmymind.net/The-Little-Go-Post")
	p2 := newPost("An Introduction to Programming in Go", "http://www.golang-post.com/")
	p3 := newPost("xxx", "zzz")

	// insert
	{
		// insert rows - auto increment PKs will be set properly after the insert
		if err := dbmap.Insert(&p1, &p2, &p3); err != nil {
			return errors.Wrap(err, "Insert failed")
		}
	}

	c1 := Comment{PostID: p1.PostID, Content: "free book!"}
	c2 := Comment{PostID: p1.PostID, Content: "It's free"}
	c3 := Comment{PostID: p2.PostID, Content: "free book!!"}
	{
		if err := dbmap.Insert(&c1, &c2, &c3); err != nil {
			return errors.Wrap(err, "Insert failed")
		}
	}

	// fetch one
	{
		var post Post
		err := dbmap.SelectOne(
			&post,
			"select * from Post where postId = ?",
			p2.PostID,
		)
		if err != nil {
			return errors.Wrap(err, "SelectOne failed")
		}
		log.Println("p2 rows:")
		log.Printf("    0: %#+v\n", post)
	}

	// 集計
	{
		type Row struct {
			ID   int64 `db:"id"`
			Even bool  `db:"even"`
			Odd  bool  `db:"odd"`
		}
		var rows []Row
		_, err := dbmap.Select(
			&rows,
			`
select
  postId as id,
  case when postId % 2 = 0 then 1 else 0 end as even,
  case when postId % 2 = 1 then 1 else 0 end as odd
from Post
`,
		)
		if err != nil {
			return errors.Wrap(err, "Select failed")
		}
		for i, row := range rows {
			log.Printf("    %d: %#+v\n", i, row)
		}
	}

	// join
	type PostCommentView struct {
		PostID    int64
		CommentID int64
		Title     string
		Content   string
	}
	{
		query := `
SELECT
  p.postId,
  c.commentId,
  p.title,
  c.content
FROM
  Post as p join Comment as c on p.postId = c.postId
`
		var rows []PostCommentView
		log.Println("----------------------------------------")
		if _, err := dbmap.Select(&rows, query); err != nil {
			return errors.Wrap(err, "Join failed")
		}
		for i, row := range rows {
			log.Printf("    %d: %#+v\n", i, row)
		}
	}
	return nil
}

func newPost(title, url string) Post {
	return Post{
		Published: time.Now().UnixNano(),
		Title:     title,
		Url:       url,
	}
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("!!+%v", err)
	}
}
