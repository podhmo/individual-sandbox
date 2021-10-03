package configanotherDB

import (
	"m/05subconfig/lib/db"
)

type AnotherDBConfig struct {
	Config db.Config `json:"anotherDB"`
}

func (c *AnotherDBConfig) AnotherDB() *db.DB {
	return c.Config.New()
}
