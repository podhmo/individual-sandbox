package main

import (
	"bytes"
	"context"
	"database/sql"
	"fmt"
	"log"
	"os"

	_ "github.com/mattn/go-sqlite3"
	sqldblogger "github.com/simukti/sqldb-logger"
)

type logAdapter struct {
	Logger *log.Logger
}

func (a *logAdapter) Log(
	ctx context.Context,
	level sqldblogger.Level,
	msg string,
	data map[string]interface{},
) {
	var b bytes.Buffer
	fmt.Fprintf(&b, "level:%s	msg:%s", level, msg)
	for k, v := range data {
		fmt.Fprintf(&b, "	%v:%v", k, v)
	}
	a.Logger.Println(b.String())
}

func main() {
	driver := os.Getenv("DRIVER")
	if driver == "" {
		driver = "sqlite3"
	}
	dbSource := os.Getenv("DB")
	if dbSource == "" {
		dbSource = "sample.db"
	}

	var db *sql.DB
	db, err := sql.Open(driver, dbSource)
	if err != nil {
		panic(err.Error()) // Just for example purpose. You should use proper error handling instead of panic
	}
	defer db.Close()

	db = sqldblogger.OpenDriver(
		dbSource,
		db.Driver(),
		&logAdapter{Logger: log.New(os.Stdout, "**", 0)},
	)

	// Prepare statement for inserting data
	stmtIns, err := db.Prepare("INSERT INTO squareNum VALUES( ?, ? )") // ? = placeholder
	if err != nil {
		panic(err.Error()) // proper error handling instead of panic in your app
	}
	defer stmtIns.Close() // Close the statement when we leave main() / the program terminates

	// Prepare statement for reading data
	stmtOut, err := db.Prepare("SELECT squareNumber FROM squarenum WHERE number = ?")
	if err != nil {
		panic(err.Error()) // proper error handling instead of panic in your app
	}
	defer stmtOut.Close()

	// Insert square numbers for 0-24 in the database
	for i := 0; i < 25; i++ {
		_, err = stmtIns.Exec(i, (i * i)) // Insert tuples (i, i^2)
		if err != nil {
			panic(err.Error()) // proper error handling instead of panic in your app
		}
	}

	var squareNum int // we "scan" the result in here

	// Query the square-number of 13
	err = stmtOut.QueryRow(13).Scan(&squareNum) // WHERE number = 13
	if err != nil {
		panic(err.Error()) // proper error handling instead of panic in your app
	}
	fmt.Printf("The square number of 13 is: %d\n", squareNum)

	// Query another number.. 1 maybe?
	err = stmtOut.QueryRow(1).Scan(&squareNum) // WHERE number = 1
	if err != nil {
		panic(err.Error()) // proper error handling instead of panic in your app
	}
	fmt.Printf("The square number of 1 is: %d\n", squareNum)
}
