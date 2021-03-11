package main

import (
	"context"
	"database/sql"
	"database/sql/driver"
	"fmt"
	"log"
	"os"

	_ "github.com/mattn/go-sqlite3"
	"golang.org/x/xerrors"
)

func init(){
	log.SetFlags(0)
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
	log.Println("\tprepare:", query)
	stmt, err := c.Conn.Prepare(query)
	if err != nil {
		return nil, err
	}
	return &wStmt{Stmt: stmt}, nil
}
func (c *wConn) Close() error {
	log.Println("\tclose:")
	return c.Conn.Close()
}
func (c *wConn) Begin() (driver.Tx, error) {
	log.Println("\tbegin:")
	tx, err := c.Conn.Begin()
	if err != nil {
		return nil, err
	}
	return &wTx{Tx: tx}, nil
}

type wTx struct {
	driver.Tx
}

func (tx *wTx) Commit() error {
	log.Println("\tcommit:")
	return tx.Tx.Commit()
}
func (tx *wTx) Rollback() error {
	log.Println("\trollback:")
	return tx.Tx.Rollback()
}

type wStmt struct {
	driver.Stmt
}

func (s *wStmt) Close() error {
	log.Println("\tclose:")
	return s.Stmt.Close()
}
func (s *wStmt) NumInput() int {
	log.Println("\tnum-input:")
	return s.Stmt.NumInput()
}
func (s *wStmt) Exec(args []driver.Value) (driver.Result, error) {
	log.Println("\texec:", args)
	result, err := s.Stmt.Exec(args)
	if err != nil {
		return nil, err
	}
	return &wResult{Result: result}, nil
}
func (s *wStmt) Query(args []driver.Value) (driver.Rows, error) {
	log.Println("\tquery:", args)
	rows, err := s.Stmt.Query(args)
	if err != nil {
		return nil, err
	}
	return &wRows{Rows: rows}, nil
}

type wResult struct {
	driver.Result
}

func (r *wResult) LastInsertId() (int64, error) {
	n, err := r.Result.LastInsertId()
	if err != nil {
		return n, err
	}
	log.Println("\tlast insert id", n)
	return n, err
}
func (r *wResult) RowsAffected() (int64, error) {
	n, err := r.Result.RowsAffected()
	if err != nil {
		return n, err
	}
	log.Println("\trows affected", n)
	return n, err
}

type wRows struct {
	driver.Rows
}

func (rows *wRows) Columns() []string {
	r := rows.Rows.Columns()
	log.Println("\t\tcolumns:", r)
	return r
}
func (rows *wRows) Close() error {
	log.Println("\t\tclose:")
	return rows.Rows.Close()
}
func (rows *wRows) Next(dest []driver.Value) error {
	log.Println("\t\tnext:", dest)
	return rows.Rows.Next(dest)
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("!!%+v", err)
	}
}

func run() error {
	dbname := os.Getenv("DB")
	{
		db, err := sql.Open("sqlite3", dbname)
		if err != nil {
			return xerrors.Errorf("sql open %w", err)
		}
		dri := db.Driver()
		if err = db.Close(); err != nil {
			return xerrors.Errorf("sql close %w", err)
		}
		sql.Register("wsqlite3", &wDriver{Driver: dri})
	}

	// Connect to a SQLite3 database using the ocsql driver wrapper.
	db, err := sql.Open("wsqlite3", dbname)
	if err != nil {
		return xerrors.Errorf("sql open %w", err)
	}

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
