package main

import (
	"context"
	"fmt"
	"log"

	"github.com/jmoiron/sqlx"
	_ "github.com/mattn/go-sqlite3"
	"github.com/pkg/errors"
)

func main() {
	ctx := context.Background()
	if err := run(ctx); err != nil {
		log.Fatalf("!!%+v", err)
	}
}

func run(ctx context.Context) error {
	db, err := sqlx.ConnectContext(ctx, "sqlite3", ":memory:")
	if err != nil {
		return err
	}

	// create table
	{
		schema := `
CREATE TABLE person (
    first_name text,
    last_name text,
    email text
);
`
		result := db.MustExecContext(ctx, schema)
		fmt.Println("@", "ok", result)
	}

	// insert data
	{
		tx := db.MustBegin()
		query := "INSERT INTO person (first_name, last_name, email) VALUES ($1, $2, $3)"
		{
			fmt.Println("@@", query, "Jason", "Moiron", "jmoiron@jmoiron.net")
			tx.MustExec(query, "Jason", "Moiron", "jmoiron@jmoiron.net")
		}
		{
			fmt.Println("@@", query, "John", "Doe", "johndoeDNE@gmail.net")
			tx.MustExec(query, "John", "Doe", "johndoeDNE@gmail.net")
		}

		tx.Commit()
		fmt.Println("@@", "ok")
	}

	// select count
	{
		var c int
		query := "SELECT COUNT(*) FROM person ORDER BY first_name ASC"
		fmt.Println("@@@", query)

		err := db.GetContext(ctx, &c, query)
		if err != nil {
			return errors.Wrap(err, "select count")
		}
		fmt.Println("@@@", "count", c)
		fmt.Println("@@@", "ok")
	}
	return nil
}
