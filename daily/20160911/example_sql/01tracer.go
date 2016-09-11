package main

import (
	"database/sql"
	"database/sql/driver"
	sqlite3 "github.com/mattn/go-sqlite3"
	"github.com/shogo82148/go-sql-proxy"
	"log"
)

type Preparer interface {
	Prepare(query string) (*sql.Stmt, error)
}

func init() {
	sql.Register("sqlite3-proxy", proxy.NewProxy(&sqlite3.SQLiteDriver{}, &proxy.Hooks{
		Open: func(_ interface{}, conn driver.Conn) error {
			log.Println("Open")
			return nil
		},
		Exec: func(_ interface{}, stmt *proxy.Stmt, args []driver.Value, result driver.Result) error {
			log.Printf("Exec: %s; args = %v\n", stmt.QueryString, args)
			return nil
		},
		Query: func(_ interface{}, stmt *proxy.Stmt, args []driver.Value, rows driver.Rows) error {
			log.Printf("Query: %s; args = %v\n", stmt.QueryString, args)
			return nil
		},
		Begin: func(_ interface{}, conn *proxy.Conn) error {
			log.Println("Begin")
			return nil
		},
		Commit: func(_ interface{}, tx *proxy.Tx) error {
			log.Println("Commit")
			return nil
		},
		Rollback: func(_ interface{}, tx *proxy.Tx) error {
			log.Println("Rollback")
			return nil
		},
	}))

	db, err := sql.Open("sqlite3-proxy", ":memory:")
	if err != nil {
		log.Fatalf("Open filed: %v", err)
	}
	defer db.Close()

	_, err = db.Exec(
		"CREATE TABLE t1 (id INTEGER PRIMARY KEY)",
	)
	if err != nil {
		log.Fatal(err)
	}
}

func setup(db *sql.DB) error {
	// The database/sql doesn’t explicitly have multiple statement support,
	// which means that the behavior of this is backend dependent:
	// (see: http://go-database-sql.org/surprises.html)

	if _, err := db.Exec(`
CREATE TABLE world (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  country VARCHAR(255),
  capital VARCHAR(255)
)
`); err != nil {
		return err
	}
	return nil
}

func run(db Preparer) error {
	// insertion
	{
		stmt, err := db.Prepare(`
INSERT INTO world (country, capital) VALUES (?, ?)
`)
		if err != nil {
			return err
		}
		for _, c := range []struct{ country, capital string }{
			{country: "日本", capital: "東京"},
			{country: "アメリカ", capital: "ワシントンD.C."},
			{country: "ロシア", capital: "モスクワ"},
			{country: "イギリス", capital: "ロンドン"},
			{country: "オーストラリア", capital: "シドニー"},
		} {
			_, err = stmt.Exec(c.country, c.capital)
			if err != nil {
				return err
			}
		}

	}

	// querying
	{
		stmt, err := db.Prepare(`
select * from world where country = ?
`)
		if err != nil {
			return err
		}

		r, err := stmt.Query("日本")

		{
			var id int
			var country, capital string
			for r.Next() {
				err = r.Scan(&id, &country, &capital)
				if err != nil {
					panic(err)
				}
				log.Printf("id=%d, country=%q, capital=%q\n", id, country, capital)
			}

		}
	}
	return nil
}

func main() {
	dbfile := ":memory:"
	db, err := sql.Open("sqlite3-proxy", dbfile)
	if err != nil {
		log.Fatal(err)
	}
	defer db.Close()

	err = setup(db)
	if err != nil {
		log.Fatal(err)
	}
	tx, err := db.Begin()
	if err != nil {
		log.Fatal(err)
	}
	defer tx.Rollback()
	err = run(tx)
	if err != nil {
		log.Fatal(err)
	}
}
