package main

import (
	"context"
	"fmt"
	"io"
	"log"
	"os"

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
	if err := SectionWith("create-table", func(w io.Writer) error {
		schema := `
CREATE TABLE person (
    first_name text,
    last_name text,
    email text
);
`
		result := db.MustExecContext(ctx, schema)
		fmt.Fprintln(w, "ok", result)
		return nil
	}); err != nil {
		return errors.Wrap(err, "create table")
	}

	if err := SectionWith("insert table", func(w io.Writer) error {
		tx := db.MustBegin()
		defer tx.Commit()

		tx.MustExec("INSERT INTO person (first_name, last_name, email) VALUES ($1, $2, $3)", "Jason", "Moiron", "jmoiron@jmoiron.net")
		tx.MustExec("INSERT INTO person (first_name, last_name, email) VALUES ($1, $2, $3)", "John", "Doe", "johndoeDNE@gmail.net")
		return nil
	}); err != nil {
		return errors.Wrap(err, "insert table")
	}

	return nil
}

type Section struct {
	msg string
}

func SectionWith(msg string, fn func(io.Writer) error) error {
	s := NewSection(msg)
	defer s.Close()
	return fn(s)
}

func NewSection(msg string) *Section {
	fmt.Fprintf(os.Stderr, "start %s\n", msg)
	return &Section{msg: msg}
}
func (s *Section) Close() {
	fmt.Fprintf(os.Stderr, "end   %s\n", s.msg)
}
func (s *Section) Write(b []byte) (int, error) {
	n, err := io.WriteString(os.Stderr, "	")
	if err != nil {
		return n, err
	}
	m, err := os.Stderr.Write(b)
	return n + m, err
}
