package main

import (
	"database/sql"
	"fmt"
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

	// insert
	{
		// insert rows - auto increment PKs will be set properly after the insert
		if err := dbmap.Insert(&b1, &b2); err != nil {
			return errors.Wrap(err, "Insert failed")
		}
	}

	// count
	{
		c, err := dbmap.SelectInt("select count(*) from Book")
		if err != nil {
			return errors.Wrap(err, "select count(*) failed")
		}
		log.Printf("rows after inserting: %v", c)
	}

	// update
	{
		b2.Title = fmt.Sprintf(`%s (updated)`, b2.Title)
		c, err := dbmap.Update(&b2)
		if err != nil {
			return errors.Wrap(err, "Update failed")
		}
		log.Println("Rows updated:", c)
	}

	// fetch one
	{
		var book Book
		err := dbmap.SelectOne(&book, "select * from Book where bookId = ?", b2.BookID)
		if err != nil {
			return errors.Wrap(err, "SelectOne failed")
		}
		log.Println("p2 rows:")
		log.Printf("    0: %#+v\n", book)
	}

	// named
	{
		var book Book
		err := dbmap.SelectOne(&book, "select * from Book where bookId = :bookId", map[string]interface{}{"bookId": b2.BookID})
		if err != nil {
			return errors.Wrap(err, "SelectOne failed")
		}
		log.Println("p2 rows:")
		log.Printf("    0: %#+v\n", book)
	}

	// fetch all
	{
		var books []Book
		_, err := dbmap.Select(&books, "select * from Book order by bookId")
		if err != nil {
			return errors.Wrap(err, "Select failed")
		}
		log.Println("All rows:")
		for x, p := range books {
			log.Printf("    %d: %#+v\n", x, p)
		}
	}

	// delete by pk
	{
		c, err := dbmap.Delete(&b1)
		if err != nil {
			return errors.Wrap(err, "Delete failed")
		}
		log.Println("Rows deleted:", c)
	}

	// delete row manually via Exec
	{
		c, err := dbmap.Exec("delete from Book where bookId=?", b2.BookID)
		if err != nil {
			return errors.Wrap(err, "Exec failed")
		}
		log.Println("Rows deleted:", c)
	}

	// confirm count is zero
	{
		c, err := dbmap.SelectInt("select count(*) from Book")
		if err != nil {
			return errors.Wrap(err, "select count(*) failed")
		}
		log.Println("Row count - should be zero:", c)

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
