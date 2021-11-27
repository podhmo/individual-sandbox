package main

import (
	"context"
	"database/sql"
	"log"
	"m/tutorial"
	"reflect"

	"gopkg.in/guregu/null.v4"
	_ "github.com/lib/pq"
)


// need:
// $ docker compose up
// $ psql -h localhost -p 5432 -U postgres -b -f schema.sql

func run() error {
	ctx := context.Background()
	db, err := sql.Open("postgres", "user=postgres host=127.0.0.1 port=5432 password=testtest dbname=postgres sslmode=disable")
	if err != nil {
		return err
	}

	queries := tutorial.New(db)

	// list all authors
	authors, err := queries.ListAuthors(ctx)
	if err != nil {
		return err
	}
	log.Println(authors)

	// create an author
	insertedAuthor, err := queries.CreateAuthor(ctx, tutorial.CreateAuthorParams{
		Name: "Brian Kernighan",
		Bio:  null.StringFrom("Co-author of The C Programming Language and The Go Programming Language"),
	})
	if err != nil {
		return err
	}
	log.Println(insertedAuthor)

	// get the author we just inserted
	fetchedAuthor, err := queries.GetAuthor(ctx, insertedAuthor.ID)
	if err != nil {
		return err
	}

	// prints true
	log.Println(reflect.DeepEqual(insertedAuthor, fetchedAuthor))
	return nil
}

func main() {
	if err := run(); err != nil {
		log.Fatal(err)
	}
}
