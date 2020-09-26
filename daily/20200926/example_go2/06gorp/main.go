package main

import (
	"database/sql"
	"log"
	"time"

	gorp "github.com/go-gorp/gorp/v3"
	_ "github.com/mattn/go-sqlite3"
	"golang.org/x/xerrors"
)

func main() {
	if err := run(); err != nil {
		log.Printf("!! %+v", err)
	}
}

func run() error {
	// initialize the DbMap
	dbmap, err := initDb()
	if err != nil {
		return err
	}
	defer dbmap.Db.Close()

	// delete any existing rows
	if err := dbmap.TruncateTables(); err != nil {
		return xerrors.Errorf("TruncateTables failed: %w", err)
	}

	// create two posts
	p1 := newPost("Go 1.1 released!", "Lorem ipsum lorem ipsum")
	p2 := newPost("Go 1.2 released!", "Lorem ipsum lorem ipsum")

	// insert rows - auto increment PKs will be set properly after the insert
	err = dbmap.Insert(&p1, &p2)
	if err != nil {
		return xerrors.Errorf("Insert failed: %w", err)
	}

	// use convenience SelectInt
	count, err := dbmap.SelectInt("select count(*) from posts")
	if err != nil {
		return xerrors.Errorf("select count(*) failed: %w", err)
	}
	log.Println("Rows after inserting:", count)

	// update a row
	p2.Title = "Go 1.2 is better than ever"
	count, err = dbmap.Update(&p2)
	if err != nil {
		return xerrors.Errorf("Update failed: %w", err)
	}
	log.Println("Rows updated:", count)

	// fetch one row - note use of "post_id" instead of "Id" since column is aliased
	//
	// Postgres users should use $1 instead of ? placeholders
	// See 'Known Issues' below
	//
	err = dbmap.SelectOne(&p2, "select * from posts where post_id=?", p2.Id)
	if err != nil {
		return xerrors.Errorf("SelectOne failed: %w", err)
	}
	log.Println("p2 row:", p2)

	// fetch all rows
	var posts []Post
	_, err = dbmap.Select(&posts, "select * from posts order by post_id")
	if err != nil {
		return xerrors.Errorf("Select failed: %w", err)
	}
	log.Println("All rows:")
	for x, p := range posts {
		log.Printf("    %d: %v\n", x, p)
	}

	// delete row by PK
	count, err = dbmap.Delete(&p1)
	if err != nil {
		return xerrors.Errorf("Delete failed: %w", err)
	}
	log.Println("Rows deleted:", count)

	// delete row manually via Exec
	_, err = dbmap.Exec("delete from posts where post_id=?", p2.Id)
	if err != nil {
		return xerrors.Errorf("Exec failed: %w", err)
	}

	// confirm count is zero
	count, err = dbmap.SelectInt("select count(*) from posts")
	if err != nil {
		return xerrors.Errorf("select count(*) failed: %w", err)
	}
	log.Println("Row count - should be zero:", count)

	log.Println("Done!")

	return nil
}

type Post struct {
	// db tag lets you specify the column name if it differs from the struct field
	Id      int64 `db:"post_id"`
	Created int64
	Title   string `db:",size:50"`               // Column size set to 50
	Body    string `db:"article_body,size:1024"` // Set both column name and size
}

func newPost(title, body string) Post {
	return Post{
		Created: time.Now().UnixNano(),
		Title:   title,
		Body:    body,
	}
}

func initDb() (*gorp.DbMap, error) {
	// connect to db using standard Go database/sql API
	// use whatever database/sql driver you wish
	db, err := sql.Open("sqlite3", ":memory:")
	if err != nil {
		return nil, xerrors.Errorf("sql.Open failed: %w", err)
	}

	// construct a gorp DbMap
	dbmap := &gorp.DbMap{Db: db, Dialect: gorp.SqliteDialect{}}

	// add a table, setting the table name to 'posts' and
	// specifying that the Id property is an auto incrementing PK
	dbmap.AddTableWithName(Post{}, "posts").SetKeys(true, "Id")

	// create the table. in a production system you'd generally
	// use a migration tool, or create the tables via scripts
	err = dbmap.CreateTablesIfNotExists()
	if err != nil {
		return nil, xerrors.Errorf("Create tables failed: %w", err)
	}

	return dbmap, nil
}
