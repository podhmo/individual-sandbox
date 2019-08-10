package main

import (
	"context"
	"fmt"
	"log"

	"github.com/jmoiron/sqlx"
	_ "github.com/mattn/go-sqlite3"
)

func main() {
	ctx := context.Background()
	db, err := sqlx.ConnectContext(ctx, "sqlite3", ":memory:")
	if err != nil {
		log.Fatalf("!!%+v", err)
	}
	fmt.Println("@", db.Ping())
}
