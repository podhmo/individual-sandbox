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

type Book struct {
	// db tag lets you specify the column name if it differs from the struct field
	BookID    int64  `db:"bookId,primarykey,autoincrement"`
	Published int64  `db:"published"`
	Title     string `db:"title,size:140"` // Column size set to 140
	Url       string `db:"url,size:1024"`  // Set both column name and size
}

func initDb() (*gorp.DbMap, error) {
	// connect to db using standard Go database/sql API
	// use whatever database/sql driver you wish
	db, err := sql.Open("sqlite3", ":memory:")
	if err != nil {
		return nil, errors.Wrap(err, "sql.Open failed")

	}

	dbmap := &gorp.DbMap{Db: db, Dialect: gorp.SqliteDialect{}}

	dbmap.AddTableWithName(Book{}, "Book").SetKeys(true, "bookId")

	err = dbmap.CreateTablesIfNotExists()
	if err != nil {
		return nil, errors.Wrap(err, "Create tables failed")
	}
	return dbmap, nil
}

func run() error {
	dbmap, err := initDb()
	if err != nil {
		return err
	}
	defer dbmap.Db.Close()

	// setup trace
	dbmap.TraceOn("*trace*", log.New(os.Stdout, "\t", 0))

	b1 := newBook("The Little Go Book", "http://openmymind.net/The-Little-Go-Book")
	b2 := newBook("An Introduction to Programming in Go", "http://www.golang-book.com/")
	b3 := newBook("xxx", "zzz")

	// insert
	{
		// insert rows - auto increment PKs will be set properly after the insert
		if err := dbmap.Insert(&b1, &b2, &b3); err != nil {
			return errors.Wrap(err, "Insert failed")
		}
	}

	// fetch one
	{
		var book Book
		err := dbmap.SelectOne(
			&book,
			"select * from Book where bookId = ?",
			b2.BookID,
		)
		if err != nil {
			return errors.Wrap(err, "SelectOne failed")
		}
		log.Println("p2 rows:")
		log.Printf("    0: %#+v\n", book)
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
  bookId as id,
  case when bookId % 2 = 0 then 1 else 0 end as even,
  case when bookId % 2 = 1 then 1 else 0 end as odd
from Book
`,
		)
		if err != nil {
			return errors.Wrap(err, "Select failed")
		}
		for i, row := range rows {
			log.Printf("    %d: %#+v\n", i, row)
		}
	}
	return nil
}

func newBook(title, url string) Book {
	return Book{
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
