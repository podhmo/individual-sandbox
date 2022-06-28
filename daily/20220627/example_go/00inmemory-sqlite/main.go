package main

import (
	"context"
	"database/sql"
	"fmt"
	"log"
	"os"
	"os/signal"
	"time"

	"github.com/pkg/errors"
	_ "modernc.org/sqlite"
)

type User struct {
	ID   int64  `db:"id,primarykey,autoincrement"`
	Name string `db:"name"`
	Age  int    `db:"age"`

	// Password string `db:"password"` // password type
	// TODO: null string, foreign key
}

func getDB(ctx context.Context, driver string, dsn string) (*sql.DB, error, func()) {
	pool, err := sql.Open(driver, dsn)
	if err != nil {
		return nil, errors.Wrap(err, "sql.Open failed"), func() {}
	}

	pool.SetConnMaxIdleTime(0)
	pool.SetMaxIdleConns(3)
	pool.SetMaxOpenConns(3)

	ctx, stop := signal.NotifyContext(ctx, os.Interrupt)
	return pool, nil, func() {
		stop()
		if err := pool.Close(); err != nil {
			log.Printf("!! DB close: %+v", err)
		}
	}
}

func initDB(ctx context.Context, db *sql.DB) error {
	{
		ctx, cancel := context.WithTimeout(ctx, 1*time.Second)
		defer cancel()
		if err := db.PingContext(ctx); err != nil {
			return fmt.Errorf("unable to connect to database: %w", err)
		}
	}

	{
		// create table
		stmt := `
	CREATE TABLE User (
		id INTEGER PRIMARY KEY AUTOINCREMENT,
		name TEXT NOT NULL,
		age INTEGER NOT NULL DEFAULT 0
	  );
	  `
		result, err := db.ExecContext(ctx, stmt)
		if err != nil {
			return fmt.Errorf("failed in create table: %w", err)
		}
		lastInsertID, err := result.LastInsertId()
		if err != nil {
			return fmt.Errorf("failed in create table: %w", err)
		}
		log.Printf("ok: %#+v", lastInsertID)
	}
	return nil
}

func doDB(ctx context.Context, db *sql.DB) error {

	// insert
	{
		users := []User{
			{Name: "foo", Age: 20},
			{Name: "bar", Age: 20},
		}
		for _, u := range users {
			stmt := `INSERT INTO User (name, age) VALUES(?, ?)`
			result, err := db.ExecContext(ctx, stmt, u.Name, u.Age)
			if err != nil {
				return fmt.Errorf("insert data: %w", err)
			}
			lastInsertID, err := result.LastInsertId()
			if err != nil {
				return fmt.Errorf("failed in create table: %w", err)
			}
			log.Printf("ok: %#+v", lastInsertID)
		}

		// TODO: bulk insert
	}

	// select
	{
		age := 20
		rows, err := db.QueryContext(ctx, "SELECT name, age FROM User WHERE age=?", age)
		if err != nil {
			return fmt.Errorf("select: %w", err)
		}
		defer rows.Close()

		var users []User
		for rows.Next() {
			var u User
			if err := rows.Scan(&u.Name, &u.Age); err != nil {
				return fmt.Errorf("scan: %w", err)
			}
			users = append(users, u)
		}

		// If the database is being written to ensure to check for Close
		// errors that may be returned from the driver. The query may
		// encounter an auto-commit error and be forced to rollback changes.
		rerr := rows.Close()
		if rerr != nil {
			return fmt.Errorf("rows.Close: %w", err)
		}

		// Rows.Err will report the last error encountered by Rows.Scan.
		if err := rows.Err(); err != nil {
			return fmt.Errorf("rows errors: %w", err)
		}

		for _, u := range users {
			fmt.Printf("%s is %d years old\n", u.Name, u.Age)
		}
	}
	return nil
}

func run() error {
	ctx := context.Background()
	db, err, teardown := getDB(ctx, "sqlite", ":memory:")
	if teardown != nil {
		defer teardown()
	}
	if err != nil {
		return err
	}
	if err := initDB(ctx, db); err != nil {
		return err
	}
	if err := doDB(ctx, db); err != nil {
		return err
	}
	return nil
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("!!+%v", err)
	}
}
