package database

import (
	"database/sql"

	gorp "github.com/go-gorp/gorp/v3"
)

type Registry struct {
	Config   *Config
	TableMap map[string]TableInfo
}

type TableInfo struct {
	Instance []interface{}
	Callback func(*gorp.TableMap)
}

func FromConfig(c *Config) (*Database, func() error, error) {
	db, err := sql.Open("sqlite3", ":memory:")
	if err != nil {
		return nil, nil, err
	}

	dbmap := &gorp.DbMap{Db: db, Dialect: gorp.SqliteDialect{}}

	// TODO:
	for name, info := range DefaultRegistry.TableMap {
		m := dbmap.AddTableWithName(info.Instance, name)
		if info.Callback != nil {
			info.Callback(m)
		}
	}

	err = dbmap.CreateTablesIfNotExists()
	if err != nil {
		defer dbmap.Db.Close()
		return nil, nil, err
	}
	return &Database{
		DbMap: dbmap,
	}, dbmap.Db.Close, nil
}

type Database struct {
	DbMap *gorp.DbMap
}
