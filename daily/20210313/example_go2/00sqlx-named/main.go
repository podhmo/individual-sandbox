package main

import (
	"log"

	"github.com/jmoiron/sqlx"
)

func main() {
	type V struct {
		K string `db:"k"`
		V int    `db:"v"`
	}

	{
		s, values, err := sqlx.Named("INSERT INTO vs (k) VALUES (:k)", []V{{K: "x"}, {K: "y"}})
		log.Printf("\n\tquery=%q\n\tvalues=%#+v\n\terr=%+v", s, values, err)
	}
	{
		s, values, err := sqlx.Named("INSERT INTO vs (k, v) VALUES (:k, :v)", []V{{K: "x"}, {K: "y"}})
		log.Printf("\n\tquery=%q\n\tvalues=%#+v\n\terr=%+v", s, values, err)
	}
}
