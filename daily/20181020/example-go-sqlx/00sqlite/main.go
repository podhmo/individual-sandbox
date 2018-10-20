package main

// from: https://github.acom/joncrlsn/go-examples/blob/master/sqlx-sqlite.go

import (
	"database/sql"
	"fmt"
	"log"

	"github.com/jmoiron/sqlx"
	_ "github.com/mattn/go-sqlite3"
)

var schema = `
DROP TABLE IF EXISTS user;
CREATE TABLE user (
	user_id    INTEGER PRIMARY KEY,
    first_name VARCHAR(80)  DEFAULT '',
    last_name  VARCHAR(80)  DEFAULT '',
	email      VARCHAR(250) DEFAULT '',
	password   VARCHAR(250) DEFAULT NULL
);
`

// User :
type User struct {
	UserId    int    `db:"user_id"`
	FirstName string `db:"first_name"`
	LastName  string `db:"last_name"`
	Email     string
	Password  sql.NullString
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func listUsers(db *sqlx.DB) error {
	people := []User{}
	if err := db.Select(&people, "SELECT * FROM user ORDER BY first_name ASC"); err != nil {
		return err
	}
	jane, jason := people[0], people[1]

	fmt.Printf("Jane: %#v\nJason: %#v\n", jane, jason)
	// User{FirstName:"Jason", LastName:"Moiron", Email:"jmoiron@jmoiron.net"}
	// User{FirstName:"John", LastName:"Doe", Email:"johndoeDNE@gmail.net"}
	return nil
}

func getUser(db *sqlx.DB) error {
	// You can also get a single result, a la QueryRow
	jason1 := User{}
	if err := db.Get(&jason1, "SELECT * FROM user WHERE first_name=$1", "Jason"); err != nil {
		return err
	}
	fmt.Printf("Jason: %#v\n", jason1)
	// User{FirstName:"Jason", LastName:"Moiron", Email:"jmoiron@jmoiron.net"}
	return nil
}

func listUsers2(db *sqlx.DB) error {
	// if you have null fields and use SELECT *, you must use sql.Null* in your struct
	users := []User{}
	if err := db.Select(&users, "SELECT * FROM user ORDER BY email ASC"); err != nil {
		return err
	}
	jane, jason, john := users[0], users[1], users[2]

	fmt.Printf("Jane: %#v\nJason: %#v\nJohn: %#v\n", jane, jason, john)
	return nil
}

func loopUsers(db *sqlx.DB) error {
	// Loop through rows using only one struct
	user := User{}
	rows, err := db.Queryx("SELECT * FROM user WHERE first_name LIKE 'J%'")
	if err != nil {
		return err
	}
	for rows.Next() {
		if err := rows.StructScan(&user); err != nil {
			return err
		}
	}
	return nil
}

func insertWithNamed(db *sqlx.DB) error {
	// Named queries, using `:name` as the bindvar.  Automatic bindvar support
	// which takes into account the dbtype based on the driverName on sqlx.Open/Connect
	if _, err := db.NamedExec(`INSERT INTO user (first_name,last_name,email) VALUES (:first,:last,:email)`,
		map[string]interface{}{
			"first": "Bin",
			"last":  "Smuth",
			"email": "bensmith@allblacks.nz",
		}); err != nil {
		return err
	}

	if _, err := db.NamedExec(`UPDATE user SET first_name=:first, last_name=:last WHERE first_name = 'Bin'`,
		map[string]interface{}{
			"first": "Ben",
			"last":  "Smith",
			"email": "bensmith@allblacks.nz",
		}); err != nil {
		return err
	}
	return nil
}

func run() error {
	// setup
	db, err := sqlx.Connect("sqlite3", ":memory:")
	if err != nil {
		return err
	}
	db.MustExec(schema)

	tx := db.MustBegin()
	tx.MustExec("INSERT INTO user (first_name, last_name, email) VALUES ($1, $2, $3)", "Jason", "Moiron", "jmoiron@jmoiron.net")
	tx.MustExec("INSERT INTO user (first_name, last_name, email, password) VALUES ($1, $2, $3, $4)", "John", "Doe", "johndoeDNE@gmail.net", "supersecret")
	// Named queries can use structs, so if you have an existing struct (i.e. person := &User{}) that you have populated, you can pass it in as &person
	tx.NamedExec("INSERT INTO user (first_name, last_name, email) VALUES (:first_name, :last_name, :email)", &User{FirstName: "Jane", LastName: "Citizen", Email: "jane.citzen@example.com"})
	tx.Commit()

	// query
	if err := listUsers(db); err != nil {
		return err
	}
	if err := getUser(db); err != nil {
		return err
	}
	if err := listUsers2(db); err != nil {
		return err
	}
	if err := loopUsers(db); err != nil {
		return err
	}

	// command
	if err := insertWithNamed(db); err != nil {
		return err
	}
	return nil
}
