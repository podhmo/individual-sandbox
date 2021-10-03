package configdb

import (
	"m/05subconfig/lib/db"
)

type DBConfig struct {
	Config db.Config `json:"db"`
}

func (c *DBConfig) DB() *db.DB {
	return c.Config.New()
}
