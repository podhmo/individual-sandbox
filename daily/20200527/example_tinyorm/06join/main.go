package main

import (
	"database/sql"
	"log"
	"m/db"
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

	// as
	{
		var rows []Comment

		c := db.Comment.As("c")
		log.Println("----------------------------------------")
		_, err := c.Query(
			db.Select(
				c.CommentID,
				c.Content,
			),
		).DoWithValues(dbmap.Select, &rows)
		if err != nil {
			return errors.Wrap(err, "select all, failed")
		}
		for i, row := range rows {
			log.Printf("    %d: %#+v\n", i, row)
		}
	}

	// join
	{
		type PostCommentView struct {
			PostID    int64
			CommentID int64
			Title     string
			Content   string
		}
		var rows []PostCommentView

		p := db.Post.As("p")
		c := db.Comment.As("c")

		_, err := p.Query(
			db.From(
				p.Join(c, db.On(p.PostID, c.PostID)),
			),
			db.Select(
				p.PostID,
				c.CommentID,
				p.Title,
				c.Content,
			),
		).DoWithValues(dbmap.Select, &rows)
		log.Println("----------------------------------------")
		if err != nil {
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
