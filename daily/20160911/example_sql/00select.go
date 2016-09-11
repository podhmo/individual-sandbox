package main

import (
	"database/sql"
	_ "github.com/mattn/go-sqlite3"
	"log"
)

func setup(db *sql.DB) error {
	if _, err := db.Exec(`
CREATE TABLE world (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  country VARCHAR(255),
  capital VARCHAR(255)
);
`); err != nil {
		return err
	}
	return nil
}

func run(db *sql.DB) error {
	// insertion
	{
		stmt, err := db.Prepare(`
INSERT INTO world (country, capital) VALUES (?, ?);
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
select * from world where country = ?;
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
	db, err := sql.Open("sqlite3", dbfile)
	if err != nil {
		log.Fatal(err)
	}
	err = setup(db)
	if err != nil {
		log.Fatal(err)
	}
	err = run(db)
	if err != nil {
		log.Fatal(err)
	}
}
