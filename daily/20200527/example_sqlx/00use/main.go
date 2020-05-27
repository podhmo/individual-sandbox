package main

import (
	"context"
	"database/sql"
	"fmt"
	"log"
	"os"
	"strings"
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

type QueryLogger struct {
	*sqlx.DB
	logger *log.Logger
}

func (p *QueryLogger) QueryContext(ctx context.Context, query string, args ...interface{}) (*sql.Rows, error) {
	fmt := strings.Repeat("\t%+v", len(args)+1)
	p.logger.Printf(fmt, append([]interface{}{query}, args...)...)
	return p.DB.QueryContext(ctx, query, args...)
}

func (p *QueryLogger) QueryxContext(ctx context.Context, query string, args ...interface{}) (*sqlx.Rows, error) {
	fmt := strings.Repeat("\t%+v", len(args)+1)
	p.logger.Printf(fmt, append([]interface{}{query}, args...)...)
	return p.DB.QueryxContext(ctx, query, args...)
}

func (p *QueryLogger) QueryRowxContext(ctx context.Context, query string, args ...interface{}) *sqlx.Row {
	fmt := strings.Repeat("\t%+v", len(args)+1)
	p.logger.Printf(fmt, append([]interface{}{query}, args...)...)
	return p.DB.QueryRowxContext(ctx, query, args...)
}
func (p *QueryLogger) ExecContext(ctx context.Context, query string, args ...interface{}) (sql.Result, error) {
	fmt := strings.Repeat("\t%+v", len(args)+1)
	p.logger.Printf(fmt, append([]interface{}{query}, args...)...)
	return p.DB.ExecContext(ctx, query, args...)
}

var schema = `
CREATE TABLE Book (
  bookId INTEGER PRIMARY KEY AUTOINCREMENT,
  published DATE,
  title TEXT,
  url TEXT
);
`

func initDb(ctx context.Context) (*QueryLogger, error) {
	// connect to db using standard Go database/sql API
	// use whatever database/sql driver you wish
	db, err := sqlx.ConnectContext(ctx, "sqlite3", ":memory:")
	if err != nil {
		return nil, errors.Wrap(err, "sql.Open failed")

	}
	// already verified in Connect()
	// if err := db.Ping(); err != nil {
	// 	return nil, errors.Wrap(err, "ping failed")
	// }

	ql := &QueryLogger{db, log.New(os.Stdout, "*trace*", 0)}

	// create table
	sqlx.MustExecContext(ctx, ql, schema)
	return ql, nil
}

func run() error {
	ctx := context.Background()

	ql, err := initDb(ctx)
	if err != nil {
		return err
	}

	// TODO: transaction
	// tx, err := db.Beginx()
	// if err != nil {
	// 	return errors.Wrap(err, "begin")
	// }
	// tx.Commit()

	b1 := newBook("The Little Go Book", "http://openmymind.net/The-Little-Go-Book")
	b2 := newBook("An Introduction to Programming in Go", "http://www.golang-book.com/")

	// insert
	{
		r, err := sqlx.NamedExecContext(ctx, ql, `INSERT INTO Book(title, url) VALUES (:title, :url)`, []*Book{&b1, &b2})
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
		err := sqlx.GetContext(ctx, ql, &c, "select count(*) from Book")
		if err != nil {
			return errors.Wrap(err, "select count(*) failed")
		}
		log.Printf("rows after inserting: %v", c)
	}

	// update
	{
		b2.Title = fmt.Sprintf(`%s (updated)`, b2.Title)
		r, err := ql.ExecContext(ctx, "update Book set title=$1 where bookId=$2", b2.Title, b2.BookID)
		if err != nil {
			return errors.Wrap(err, "Update failed")
		}
		affected, err := r.RowsAffected()
		if err != nil {
			return errors.Wrap(err, "Update failed")
		}
		log.Println("Rows updated:", affected)
	}

	// fetch one
	{
		var book Book
		if err := sqlx.GetContext(ctx, ql, &book, "select * from Book where bookId = $1", b2.BookID); err != nil {
			return errors.Wrap(err, "SelectOne failed")
		}
		log.Println("p2 rows:")
		log.Printf("    0: %#+v\n", book)
	}

	// fetch all
	{
		var books []Book
		err := sqlx.SelectContext(ctx, ql, &books, "select * from Book order by bookId")
		if err != nil {
			return errors.Wrap(err, "Select failed")
		}
		log.Println("All rows:")
		for x, p := range books {
			log.Printf("    %d: %#+v\n", x, p)
		}
	}

	// iterate
	{
		var book Book
		rows, err := sqlx.NamedQueryContext(ctx, ql, "select * from Book order by bookId", map[string]interface{}{})
		if err != nil {
			return errors.Wrap(err, "Select failed")
		}

		log.Println("Iterate rows:")
		for i := 0; rows.Next(); i++ {
			err := rows.StructScan(&book)
			if err != nil {
				return errors.Wrapf(err, "scan %d", i)
			}
			log.Printf("    %d: %#+v\n", i, book)
		}
	}

	// // delete by pk
	// {
	// 	c, err := db.Delete(&b1)
	// 	if err != nil {
	// 		return errors.Wrap(err, "Delete failed")
	// 	}
	// 	log.Println("Rows deleted:", c)
	// }

	// // delete row manually via Exec
	// {
	// 	c, err := db.Exec("delete from Book where bookId=?", b2.BookID)
	// 	if err != nil {
	// 		return errors.Wrap(err, "Exec failed")
	// 	}
	// 	log.Println("Rows deleted:", c)
	// }

	// // confirm count is zero
	// {
	// 	c, err := db.SelectInt("select count(*) from Book")
	// 	if err != nil {
	// 		return errors.Wrap(err, "select count(*) failed")
	// 	}
	// 	log.Println("Row count - should be zero:", c)

	// }
	return nil
}

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
