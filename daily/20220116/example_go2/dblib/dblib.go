package dblib

import (
	"database/sql"
	"os"

	"github.com/rs/zerolog"
	sqldblogger "github.com/simukti/sqldb-logger"
	"github.com/simukti/sqldb-logger/logadapter/zerologadapter"
	_ "modernc.org/sqlite"
)

func SqliteOpen(dbSource string) (*sql.DB, func() error, error) {
	return open("sqlite", dbSource)
}
func open(driver, dbSource string) (*sql.DB, func() error, error) {
	var db *sql.DB
	db, err := sql.Open(driver, dbSource)
	if err != nil {
		return nil, nil, err
	}

	logger := zerolog.New(
		zerolog.ConsoleWriter{Out: os.Stdout, NoColor: false},
	)
	// populate log pre-fields here before set to
	db = sqldblogger.OpenDriver(
		dbSource,
		db.Driver(),
		zerologadapter.New(logger),
		// optional config...
	)
	return db, db.Close, nil
}
