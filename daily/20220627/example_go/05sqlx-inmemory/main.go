package main

import (
	"context"
	"fmt"
	"log"
	"os"
	"os/signal"
	"time"

	"github.com/jmoiron/sqlx"
	_ "modernc.org/sqlite"
)

type User struct {
	ID   int64  `db:"id,primarykey,autoincrement"`
	Name string `db:"name"`
	Age  int    `db:"age"`

	// Password string `db:"password"` // password type
	// TODO: null string, foreign key
}

func getDB(ctx context.Context, driver string, dsn string) (*sqlx.DB, error, func()) {
	pool, err := sqlx.ConnectContext(ctx, driver, dsn)
	if err != nil {
		return nil, fmt.Errorf("connect db: %w", err), func() {}
	}
	pool.SetConnMaxIdleTime(0)
	pool.SetMaxIdleConns(3)
	pool.SetMaxOpenConns(3)

	return pool, nil, func() {
		if err := pool.Close(); err != nil {
			log.Printf("!! DB close: %+v", err)
		}
	}
}

func initDB(ctx context.Context, db *sqlx.DB) error {
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

func doDB(ctx context.Context, db *sqlx.DB) error {

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

	// select one
	{
		name := "foo"
		var u User
		if err := db.GetContext(ctx, &u, "SELECT name, age FROM User WHERE name=?", name); err != nil {
			return fmt.Errorf("select: %w", err)
		}
		fmt.Printf("%s is %d years old\n", u.Name, u.Age)
	}

	fmt.Println("----------------------------------------")

	// select many
	{
		age := 20
		var users []User
		if err := db.SelectContext(ctx, &users, "SELECT name, age FROM User WHERE age=?", age); err != nil {
			return fmt.Errorf("select: %w", err)
		}
		for _, u := range users {
			fmt.Printf("%s is %d years old\n", u.Name, u.Age)
		}
	}
	return nil
}

func run() error {
	ctx := context.Background()
	ctx, stop := signal.NotifyContext(ctx, os.Interrupt)
	defer stop()

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
