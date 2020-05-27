package main

import (
	"context"
	"database/sql"
	"log"
	"m/miniq"
	"time"

	"github.com/jmoiron/sqlx"
	_ "github.com/mattn/go-sqlite3"
	"github.com/pkg/errors"
)

// from https://github.com/go-gorp/gorp

type Book struct {
	// db tag lets you specify the column name if it differs from the struct field
	BookID    int64         `db:"bookId,primarykey,autoincrement"`
	Published sql.NullInt64 `db:"published"`
	Title     string        `db:"title,size:140"` // Column size set to 140
	Url       string        `db:"url,size:1024"`  // Set both column name and size
}

var schema = `
CREATE TABLE Book (
  bookId INTEGER PRIMARY KEY AUTOINCREMENT,
  published DATE,
  title TEXT,
  url TEXT
);
`

func initDb(ctx context.Context) (*sqlx.DB, error) {
	// connect to db using standard Go database/sql API
	// use whatever database/sql driver you wish
	db, err := sqlx.ConnectContext(ctx, "sqlite3", ":memory:")
	if err != nil {
		return nil, errors.Wrap(err, "sql.Open failed")

	}

	// create table
	db.MustExecContext(ctx, schema)
	return db, nil
}

func run() error {
	ctx := context.Background()

	db, err := initDb(ctx)
	if err != nil {
		return err
	}

	b1 := newBook("The Little Go Book", "http://openmymind.net/The-Little-Go-Book")
	b2 := newBook("An Introduction to Programming in Go", "http://www.golang-book.com/")

	// insert
	{
		r, err := db.NamedExecContext(ctx, `INSERT INTO Book(title, url) VALUES (:title, :url)`, []*Book{&b1, &b2})
		// insert rows - auto increment PKs will be set properly after the insert
		if err != nil {
			return errors.Wrap(err, "Insert failed")
		}
		id, err := r.LastInsertId()
		if err != nil {
			return errors.Wrap(err, "Insert failed")
		}
		b2.BookID = id
	}

	// count
	{
		var c int
		err := db.GetContext(ctx, &c, "select count(*) from Book")
		if err != nil {
			return errors.Wrap(err, "select count(*) failed")
		}
		log.Printf("rows after inserting: %v", c)
	}

	// // update
	// {
	// 	b2.Title = fmt.Sprintf(`%s (updated)`, b2.Title)
	// 	r, err := db.ExecContext(ctx, "update Book set title=$1 where bookId=$2", b2.Title, b2.BookID)
	// 	if err != nil {
	// 		return errors.Wrap(err, "Update failed")
	// 	}
	// 	affected, err := r.RowsAffected()
	// 	if err != nil {
	// 		return errors.Wrap(err, "Update failed")
	// 	}
	// 	log.Println("Rows updated:", affected)
	// }

	// fetch one
	{
		var book Book
		err := miniq.Query(
			miniq.Select(miniq.STAR),
			miniq.From(miniq.Table("Book")),
			miniq.Where(BookID.Compare("= ?", b2.BookID)),
		).Do(db.Get, &book)
		if err != nil {
			return errors.Wrap(err, "SelectOne failed")
		}
		log.Println("p2 rows:")
		log.Printf("    0: %#+v\n", book)
	}

	// fetch all
	{
		var books []Book
		err := miniq.Query(
			miniq.Select(miniq.STAR),
			miniq.From(miniq.Table("Book")),
			miniq.Where(),
		).Do(db.Select, &books)
		if err != nil {
			return errors.Wrap(err, "Select failed")
		}
		log.Println("All rows:")
		for x, p := range books {
			log.Printf("    %d: %#+v\n", x, p)
		}
	}

	// // iterate
	// {
	// 	var book Book
	// 	rows, err := db.NamedQueryContext(ctx, "select * from Book order by bookId", map[string]interface{}{})
	// 	if err != nil {
	// 		return errors.Wrap(err, "Select failed")
	// 	}

	// 	log.Println("Iterate rows:")
	// 	for i := 0; rows.Next(); i++ {
	// 		err := rows.StructScan(&book)
	// 		if err != nil {
	// 			return errors.Wrapf(err, "scan %d", i)
	// 		}
	// 		log.Printf("    %d: %#+v\n", i, book)
	// 	}
	// }

	return nil
}

var (
	BookID    = miniq.Int64Field("bookId")
	Published = miniq.Int64Field("published")
	URL       = miniq.StringField("url")
)

func newBook(title, url string) Book {
	return Book{
		Published: sql.NullInt64{Int64: time.Now().UnixNano()},
		Title:     title,
		Url:       url,
	}
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("!!+%v", err)
	}
}
