package database

import (
	"database/sql"
	"m/model"

	"github.com/go-gorp/gorp/v3"
	"golang.org/x/xerrors"
)

type gateway struct {
	db *DB
}

type DB struct {
	dburl string
	*gorp.DbMap

	common gateway
	Post   *PostStore
}

func New(dburl string) (*DB, error, func() error) {
	db, err := sql.Open("sqlite3", dburl)
	if err != nil {
		return nil, xerrors.Errorf("sql.Open failed: %w", err), nil
	}

	dbmap := &gorp.DbMap{Db: db, Dialect: gorp.SqliteDialect{}}
	dbmap.AddTableWithName(model.Post{}, "posts").SetKeys(true, "Id")

	err = dbmap.CreateTablesIfNotExists()
	if err != nil {
		return nil, xerrors.Errorf("Create tables failed: %w", err), nil
	}

	client := &DB{
		dburl: dburl,
		DbMap: dbmap,
	}
	client.common.db = client
	client.Post = (*PostStore)(&client.common)
	return client, nil, client.DbMap.Db.Close
}
