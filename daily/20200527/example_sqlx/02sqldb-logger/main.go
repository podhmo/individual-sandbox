package main

import (
	"context"
	"database/sql"
	"fmt"
	"log"
	"time"

	"github.com/jmoiron/sqlx"
	_ "github.com/mattn/go-sqlite3"
	"github.com/pkg/errors"
	sqldblogger "github.com/simukti/sqldb-logger"
	"github.com/simukti/sqldb-logger/logadapter/zapadapter"
	"go.uber.org/zap"
	"go.uber.org/zap/zapcore"
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
	rdb, err := sql.Open("sqlite3", ":memory:")
	if err != nil {
		return nil, errors.Wrap(err, "sql.Open failed")
	}

	config := zap.NewDevelopmentConfig()
	config.EncoderConfig.EncodeLevel = zapcore.CapitalColorLevelEncoder
	config.Level = zap.NewAtomicLevelAt(zap.DebugLevel) // whatever minimum level
	config.DisableCaller = true
	logger, _ := config.Build()
	defer logger.Sync()

	loggerAdapter := zapadapter.New(logger)
	rdb = sqldblogger.OpenDriver(":memory:", rdb.Driver(), loggerAdapter /*, using_default_options*/) // db is STILL *sql.DB
	rdb.Ping()

	// connect to db using standard Go database/sql API
	// use whatever database/sql driver you wish
	db := sqlx.NewDb(rdb, "sqlite3")

	// create table
	db.MustExecContext(ctx, schema)
	return db, nil
}

func run() error {
	ctx := context.Background()
	log.SetFlags(0)

	db, err := initDb(ctx)
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

	// update
	{
		b2.Title = fmt.Sprintf(`%s (updated)`, b2.Title)
		r, err := db.ExecContext(ctx, "update Book set title=$1 where bookId=$2", b2.Title, b2.BookID)
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
		if err := db.GetContext(ctx, &book, "select * from Book where bookId = $1", b2.BookID); err != nil {
			return errors.Wrap(err, "SelectOne failed")
		}
		log.Println("p2 rows:")
		log.Printf("    0: %#+v\n", book)
	}

	// fetch all
	{
		var books []Book
		err := db.SelectContext(ctx, &books, "select * from Book order by bookId")
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
		rows, err := db.NamedQueryContext(ctx, "select * from Book order by bookId", map[string]interface{}{})
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
