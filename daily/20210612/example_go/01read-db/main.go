package main

import (
	"context"
	"database/sql"
	"fmt"
	"log"
	"os"

	"github.com/jmoiron/sqlx"
	"github.com/mattn/go-sqlite3"
	_ "github.com/mattn/go-sqlite3"
	"github.com/pkg/errors"
	"github.com/rs/zerolog"
	sqldblogger "github.com/simukti/sqldb-logger"
	"github.com/simukti/sqldb-logger/logadapter/zerologadapter"
)

type Row struct {
	Index          int64          `db:"index"`
	No             int64          `db:"図鑑番号"`
	Name           string         `db:"ポケモン名"`
	Type1          string         `db:"タイプ１"`
	Type2          sql.NullString `db:"タイプ２"`
	Ability1       string         `db:"通常特性１"`
	Ability2       sql.NullString `db:"通常特性２"`
	HiddenAbility  sql.NullString `db:"夢特性"`
	HP             int64          `db:"HP"`
	Attack         int64          `db:"こうげき"`
	Defence        int64          `db:"ぼうぎょ"`
	SpecialAttack  int64          `db:"とくこう"`
	SpecialDefence int64          `db:"とくぼう"`
	Speed          int64          `db:"すばやさ"`
	Total          int64          `db:"合計"`
}

func initDb(ctx context.Context) (*sqlx.DB, error) {
	logger := zerolog.New(
		zerolog.ConsoleWriter{Out: os.Stdout, NoColor: false},
	)

	dbname := "pokemon.db"
	if v := os.Getenv("DB"); v != "" {
		dbname = v
	}
	db := sqldblogger.OpenDriver(
		dbname,
		&sqlite3.SQLiteDriver{},
		zerologadapter.New(logger),
		// optional config...
	)

	sqlxdb := sqlx.NewDb(db, "sqlite3")
	if err := db.PingContext(ctx); err != nil {
		return nil, errors.Wrap(err, "ping failed")

	}
	return sqlxdb, nil
}

func run() error {
	ctx := context.Background()

	db, err := initDb(ctx)
	if err != nil {
		return err
	}

	// select
	{
		log.Println(`db.data.find( {} )`)
		var rows []Row
		err := db.SelectContext(ctx, &rows, "select * from data limit 10;")
		if err != nil {
			return errors.Wrap(err, "Select failed")
		}
		for i := 0; i < len(rows); i++ {
			fmt.Printf("%v\n", rows[i])
		}
		// spew.Dump(rows)
	}

	// TODO: join?, limit
	return nil
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("!!+%v", err)
	}
}
