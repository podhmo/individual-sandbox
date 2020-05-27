package main

import (
	"fmt"
	"log"
	"time"

	_ "github.com/mattn/go-sqlite3"
	"github.com/pkg/errors"
	"xorm.io/core"
	"xorm.io/xorm"
)

type Book struct {
	BookID    int64     `xorm:"bookId <- pk"` // read only
	Published time.Time `xorm:"DATE 'published'"`
	Title     string    `xorm:"'title'"` // Column size set to 140
	Url       string    `xorm:"'url'"`  // Set both column name and size
}

func (*Book) TableName() string {
	return "Book"
}

var schema = `
CREATE TABLE Book (
  bookId INTEGER PRIMARY KEY AUTOINCREMENT,
  published DATE,
  title TEXT,
  url TEXT
);
`

func run() error {
	engine, err := xorm.NewEngine("sqlite3", ":memory:")
	if err != nil {
		return err
	}

	engine.ShowSQL(true)
	engine.TZLocation, _ = time.LoadLocation("Asia/Tokyo")

	// http://gobook.io/read/gitea.com/xorm/manual-en-US/chapter-02/1.mapping.html
	engine.SetMapper(core.SameMapper{})

	_, err = engine.Exec(schema)
	if err != nil {
		return errors.Wrap(err, "init db")
	}

	b1 := newBook("The Little Go Book", "http://openmymind.net/The-Little-Go-Book")
	b2 := newBook("An Introduction to Programming in Go", "http://www.golang-book.com/")

	// insert
	{
		affected, err := engine.Insert(&b1, &b2)

		if err != nil {
			return errors.Wrap(err, "failed insert")
		}
		log.Println("affected", affected)

	}

	// count
	{
		var book Book
		c, err := engine.Count(&book)
		if err != nil {
			return errors.Wrap(err, "select count(*) failed")
		}
		log.Printf("rows after inserting: %v", c)
	}

	// update
	{
		b2.Title = fmt.Sprintf(`%s (updated)`, b2.Title)
		c, err := engine.ID(2).Cols("title").Update(&b2)
		if err != nil {
			return errors.Wrap(err, "Update failed")
		}
		log.Println("Rows updated:", c)
	}

	// select all
	{
		var books []Book
		if err := engine.Find(&books); err != nil {
			return errors.Wrap(err, "failed select all")
		}
		log.Println("All rows:")
		for x, p := range books {
			log.Printf("    %d: %#+v\n", x, p)
		}
	}
	return nil
}

func newBook(title, url string) Book {
	return Book{
		Published: time.Now(),
		Title:     title,
		Url:       url,
	}
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("!!+%v", err)
	}
}
