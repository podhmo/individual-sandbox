package main

import (
	"fmt"
	"log"
	"m/dblib"
	"os"
	"strings"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func run() (err error) {
	dbSource := os.Getenv("DB")
	if dbSource == "" {
		dbSource = "sample.db"
	}
	db, teardown, err := dblib.SqliteOpen(dbSource)
	if teardown != nil {
		defer func() {
			if rawErr := teardown(); rawErr != nil {
				err = rawErr
			}
		}()
	}
	if err != nil {
		return err
	}

	{
		N := 25
		// Prepare statement for inserting data
		stmtIns, err := db.Prepare(fmt.Sprintf("INSERT INTO squareNum VALUES %s", strings.TrimSuffix(strings.Repeat("(?, ?),", N), ","))) // ? = placeholder
		if err != nil {
			return fmt.Errorf("insert prepared statement example: %w", err)
		}
		defer stmtIns.Close() // Close the statement when we leave main() / the program terminates
		// Insert square numbers for 0-24 in the database
		args := make([]interface{}, 0, N*2)
		for i := 0; i < N; i++ {
			args = append(args, int64(i), int64(i*i))
		}
		_, err = stmtIns.Exec(args...)
		if err != nil {
			return fmt.Errorf("insert example: %w", err)
		}
	}

	{
		// Prepare statement for reading data
		stmtOut, err := db.Prepare("SELECT squareNumber FROM squarenum WHERE number = ?")
		if err != nil {
			return fmt.Errorf("select prepared statement example: %w", err)
		}
		defer stmtOut.Close()

		var squareNum int64 // we "scan" the result in here

		// Query the square-number of 13
		err = stmtOut.QueryRow(int64(13)).Scan(&squareNum) // WHERE number = 13
		if err != nil {
			return fmt.Errorf("query example: %w", err)
		}
		fmt.Printf("The square number of 13 is: %d\n", squareNum)

		// Query another number.. 1 maybe?
		err = stmtOut.QueryRow(int64(1)).Scan(&squareNum) // WHERE number = 1
		if err != nil {
			return fmt.Errorf("query example: %w", err)
		}
		fmt.Printf("The square number of 1 is: %d\n", squareNum)
	}
	return nil
}
