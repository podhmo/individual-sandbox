package main

import (
	"database/sql"
	"fmt"
	"log"
	"strings"
	"time"

	"github.com/go-gorp/gorp/v3"
	_ "github.com/mattn/go-sqlite3"
	"github.com/pkg/errors"
)

type Book struct {
	// db tag lets you specify the column name if it differs from the struct field
	BookID    int64  `db:"bookId,primarykey,autoincrement"`
	Published int64  `db:"published"`
	Title     string `db:"title,size:140"` // Column size set to 140
	URL       string `db:"url,size:1024"`  // Set both column name and size
}

// BookID(Eq(b.ID))
// BookId("%s = ?", b.ID)
func NewInt64Condition(name string) func(format string, value int64) func(*WhereClause) {
	return func(format string, value int64) func(*WhereClause) {
		return func(c *WhereClause) {
			c.Formats = append(c.Formats, fmt.Sprintf(format, name))
			c.Values = append(c.Values, value)
		}
	}
}
func NewStringCondition(name string) func(format string, value string) func(*WhereClause) {
	return func(format string, value string) func(*WhereClause) {
		return func(c *WhereClause) {
			c.Formats = append(c.Formats, fmt.Sprintf(format, name))
			c.Values = append(c.Values, value)
		}
	}
}

var (
	BookID    = NewInt64Condition("bookId")
	Published = NewInt64Condition("published")
	Title     = NewStringCondition("title")
	URL       = NewStringCondition("url")
)

type WhereClause struct {
	Formats []string
	Values  []interface{}
	Bop     string
}

func Where(options ...func(*WhereClause)) *WhereClause {
	c := &WhereClause{Bop: " And "}
	for _, opt := range options {
		opt(c)
	}
	return c
}

func (c *WhereClause) Stmt() string {
	return "WHERE " + strings.Join(c.Formats, c.Bop)
}

func (c *WhereClause) SelectOne(dbmap *gorp.DbMap, ob interface{}) error {
	// Table
	return dbmap.SelectOne(ob, "SELECT * from Book "+c.Stmt(), c.Values...)
}

func initDb() (*gorp.DbMap, error) {
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
	// dbmap.TraceOn("*trace*", log.New(os.Stdout, "\t", 0))

	b1 := newBook("The Little Go Book", "http://openmymind.net/The-Little-Go-Book")
	b2 := newBook("An Introduction to Programming in Go", "http://www.golang-book.com/")

	// insert
	{
		// insert rows - auto increment PKs will be set properly after the insert
		if err := dbmap.Insert(&b1, &b2); err != nil {
			return errors.Wrap(err, "Insert failed")
		}
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

	// named2
	{
		var book Book
		err := Where(BookID("%s = ?", b2.BookID)).SelectOne(dbmap, &book)
		if err != nil {
			return errors.Wrap(err, "SelectOne failed")
		}
		log.Println("p2 rows:")
		log.Printf("    0: %#+v\n", book)
	}
	return nil
}

func newBook(title, url string) Book {
	return Book{
		Published: time.Now().UnixNano(),
		Title:     title,
		URL:       url,
	}
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("!!+%v", err)
	}
}
