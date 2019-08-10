package main

import (
	"database/sql"
	"fmt"

	"github.com/jmoiron/sqlx"
	_ "github.com/mattn/go-sqlite3"
	"github.com/pkg/errors"
	"github.com/podhmo/ctxlog/stdctxlog"
)

type Place struct {
	Country       string
	City          sql.NullString
	TelephoneCode int `db:"telcode"`
}

func main() {
	log := stdctxlog.New(stdctxlog.WithVerbose())
	if err := run(); err != nil {
		log.With("error", err).Warning("!!")
	}
}

func run() error {
	db, err := sqlx.Open("sqlite3", ":memory:")
	if err != nil {
		return err
	}
	fmt.Println("@", db.Ping())

	// create table
	{
		schema := `CREATE TABLE place (
        country text,
        city text NULL,
        telcode integer);`

		// execute a query on the server
		result, err := db.Exec(schema)
		if err != nil {
			return err
		}
		fmt.Printf("@ %#+v\n", result)
	}

	// insert
	{
		// or, you can use MustExec, which panics on error
		cityState := `INSERT INTO place (country, telcode) VALUES (?, ?)`
		countryCity := `INSERT INTO place (country, city, telcode) VALUES (?, ?, ?)`

		db.MustExec(cityState, "Hong Kong", 852)
		db.MustExec(cityState, "Singapore", 65)
		db.MustExec(countryCity, "South Africa", "Johannesburg", 27)
	}

	// query
	{
		// fetch all places from the db
		rows, err := db.Query("SELECT country, city, telcode FROM place")
		fmt.Println("@@")
		if err != nil {
			return errors.Wrap(err, "query")
		}

		// iterate over each row
		for rows.Next() {
			var country string
			// note that city can be NULL, so we use the NullString type
			var city sql.NullString
			var telcode int
			err = rows.Scan(&country, &city, &telcode)
			if err != nil {
				return errors.Wrap(err, "query")
			}
		}
	}

	// query2
	{
		rows, err := db.Queryx("SELECT * FROM place")
		if err != nil {
			return errors.Wrap(err, "query2")
		}
		for rows.Next() {
			var p Place
			err = rows.StructScan(&p)
			if err != nil {
				return errors.Wrap(err, "query2")
			}
			fmt.Println("@@", p)
		}
	}

	// query row
	{
		row := db.QueryRow("SELECT telcode FROM place WHERE telcode=?", 852)
		var telcode int
		err = row.Scan(&telcode)
		if err != nil {
			return errors.Wrap(err, "query row")
		}
		fmt.Println("@@@", "telcode=", telcode)
	}

	// get and select
	{
		p := Place{}
		pp := []Place{}

		// this will pull the first place directly into p
		err = db.Get(&p, "SELECT * FROM place LIMIT 1")
		if err != nil {
			return errors.Wrap(err, "get and select")
		}
		fmt.Println("@@@@", p)

		// this will pull places with telcode > 50 into the slice pp
		err = db.Select(&pp, "SELECT * FROM place WHERE telcode > ?", 50)
		if err != nil {
			return errors.Wrap(err, "get and select")
		}
		fmt.Println("@@@@", pp)

		// they work with regular types as well
		var id int
		err = db.Get(&id, "SELECT count(*) FROM place")
		if err != nil {
			return errors.Wrap(err, "get and select")
		}
		fmt.Println("@@@@", id)

		// fetch at most 10 place names
		var names []string
		err = db.Select(&names, "SELECT country FROM place LIMIT 10")
		if err != nil {
			return errors.Wrap(err, "get and select")
		}
		fmt.Println("@@@@", names)
	}

	// 途中
	return nil
}
