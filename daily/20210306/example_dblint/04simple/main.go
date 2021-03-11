package main

import (
	"context"
	"database/sql"
	"database/sql/driver"
	"fmt"
	"log"
	"os"

	"github.com/jmoiron/sqlx"
	_ "github.com/mattn/go-sqlite3"
)

func init() {
	log.SetOutput(os.Stdout)
	log.SetFlags(0)
}

type wConnector struct {
	dsn    string
	driver driver.Driver
}

func (c *wConnector) Driver() driver.Driver {
	return c.driver
}
func (c *wConnector) Connect(context.Context) (driver.Conn, error) {
	conn, err := c.driver.Open(c.dsn)
	return conn, err
}

type wDriver struct {
	driver.Driver
}

func (d *wDriver) Open(name string) (driver.Conn, error) {
	conn, err := d.Driver.Open(name)
	if err != nil {
		return nil, err
	}
	return &wConn{Conn: conn}, nil
}

type wConn struct {
	driver.Conn
}

func (c *wConn) Prepare(query string) (driver.Stmt, error) {
	stmt, err := c.Conn.Prepare(query)
	if err != nil {
		return nil, err
	}
	return &wStmt{Stmt: stmt, query: query}, nil
}

type wStmt struct {
	driver.Stmt
	query string
}

func (s *wStmt) Query(args []driver.Value) (driver.Rows, error) {
	log.Printf("\tquery: %q -- %+v", s.query, args)
	rows, err := s.Stmt.Query(args)
	if err != nil {
		return nil, err
	}
	return rows, nil
}

func (s *wStmt) Exec(args []driver.Value) (driver.Result, error) {
	log.Printf("\tExec: %q -- %+v", s.query, args)
	result, err := s.Stmt.Exec(args)
	if err != nil {
		return nil, err
	}
	return result, nil
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("!!%+v", err)
	}
}

func run() error {
	// dbname := os.Getenv("DB")
	dbname := ":memory:"
	db, err := sql.Open("sqlite3", dbname)
	if err != nil {
		return fmt.Errorf("sql open %w", err)
	}
	dri := db.Driver()
	db = sql.OpenDB(&wConnector{driver: &wDriver{dri}, dsn: dbname})

	ctx := context.Background()
	if err := do(ctx, sqlx.NewDb(db, "sqlite3")); err != nil {
		return err
	}
	return nil
}

func do(ctx context.Context, db *sqlx.DB) error {
	type Inventory struct {
		ID   int64  `db:"id,primarykey,autoincrement"`
		Name string `db:"name,size:255"`
	}

	if _, err := db.ExecContext(ctx, `
CREATE TABLE inventory (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT
);`); err != nil {
		return err
	}
	if _, err := db.ExecContext(ctx, `
INSERT INTO inventory (name) VALUES ("journal"),("xxx")
`); err != nil {
		return err
	}
	{
		var x Inventory
		if err := db.Get(&x, `SELECT * FROM inventory where name = ?`, "journal"); err != nil {
			return err
		}
		fmt.Printf("%#+v\n", x)
	}
	return nil
}
