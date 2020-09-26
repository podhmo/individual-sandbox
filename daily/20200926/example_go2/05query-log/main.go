package main

import (
	"bytes"
	"context"
	"database/sql"
	"fmt"
	"log"

	_ "github.com/mattn/go-sqlite3"
	sqldblogger "github.com/simukti/sqldb-logger"
	"golang.org/x/xerrors"
)

type logger struct{}

func (l *logger) Log(ctx context.Context, level sqldblogger.Level, msg string, data map[string]interface{}) {
	w := bytes.NewBufferString(fmt.Sprintf("%s:%v", level, msg))
	// todo: sort?
	for k, v := range data {
		fmt.Fprintf(w, "\t%s:%v", k, v)
	}
	log.Println(w.String())
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("!!%+v", err)
	}
}

func run() error {
	inner, err := sql.Open("sqlite3", "resource.db")
	if err != nil {
		return xerrors.Errorf("sql open %w", err)
	}
	db := sqldblogger.OpenDriver("resource.db", inner.Driver(), &logger{})
	db.Ping()

	ctx := context.Background()
	if err := do(ctx, db); err != nil {
		return err
	}
	fmt.Println("----------------------------------------")
	if err := do(ctx, db); err != nil {
		return err
	}
	return nil
}

func do(ctx context.Context, db *sql.DB) error {
	tx, err := db.Begin()
	if err != nil {
		return xerrors.Errorf("tx begin %w", err)
	}
	defer tx.Commit() // todo: rollback

	rows, err := db.QueryContext(ctx, "select * from users")
	if err != nil {
		return xerrors.Errorf("sql query %w", err)
	}

	i := 0
	for rows.Next() {
		var val struct {
			ID   int
			Name string
		}
		err = rows.Scan(&val.ID, &val.Name)
		if err != nil {
			return xerrors.Errorf("scan %d %w", i, err)
		}
		fmt.Println(val)
		i++
	}
	return nil
}
