package main

import (
	"bytes"
	"context"
	"database/sql"
	"encoding/json"
	"fmt"
	"log"
	"m/ent"
	"m/ent/user"
	"os"

	entsql "github.com/facebook/ent/dialect/sql"
	_ "github.com/mattn/go-sqlite3"
	sqldblogger "github.com/simukti/sqldb-logger"
	"golang.org/x/xerrors"
)

type logger struct{}

func (l *logger) Log(ctx context.Context, level sqldblogger.Level, msg string, data map[string]interface{}) {
	w := bytes.NewBufferString(fmt.Sprintf("\x1b[90m%s:%v", level, msg))
	// todo: sort?
	for k, v := range data {
		fmt.Fprintf(w, "\t%s:%v", k, v)
	}
	fmt.Fprintf(w, "\x1b[0m")
	log.Println(w.String())
}

// Open new connection and start stats recorder.
func Open(dsn string) (*ent.Client, error) {
	inner, err := sql.Open("sqlite3", dsn)
	if err != nil {
		return nil, xerrors.Errorf("open: %w", err)
	}
	db := sqldblogger.OpenDriver(dsn, inner.Driver(), &logger{})

	drv := entsql.OpenDB("sqlite3", db)
	return ent.NewClient(ent.Driver(drv)), nil
}

func QueryUser(ctx context.Context, client *ent.Client) (*ent.User, error) {
	u, err := client.User.
		Query().
		Where(user.NameEQ("a8m")).
		// `Only` fails if no user found,
		// or more than 1 user returned.
		Only(ctx)
	if err != nil {
		return nil, fmt.Errorf("failed querying user: %v", err)
	}
	log.Println("user returned: ", u)
	return u, nil
}
func CreateUser(ctx context.Context, client *ent.Client) (*ent.User, error) {
	u, err := client.User.
		Create().
		SetAge(30).
		SetName("a8m").
		Save(ctx)
	if err != nil {
		return nil, fmt.Errorf("failed creating user: %v", err)
	}
	log.Println("user was created: ", u)
	return u, nil
}

func main() {
	client, err := Open("file:ent?mode=memory&cache=shared&_fk=1")
	if err != nil {
		log.Fatalf("failed opening connection to sqlite: %v", err)
	}
	defer client.Close()

	ctx := context.Background()
	// Run the auto migration tool.
	if err := client.Schema.Create(ctx); err != nil {
		log.Fatalf("failed creating schema resources: %v", err)
	}

	{
		_, err := CreateUser(ctx, client)
		if err != nil {
			log.Fatalf("failed exec: %v", err)
		}
	}

	user, err := QueryUser(ctx, client)
	if err != nil {
		log.Fatalf("failed query: %v", err)
	}

	encoder := json.NewEncoder(os.Stdout)
	encoder.SetIndent("", "  ")
	encoder.Encode(user)
}
