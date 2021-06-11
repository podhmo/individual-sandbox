package main

import (
	"context"
	"database/sql"
	"fmt"
	"log"
	"os"
	"strings"

	"github.com/jmoiron/sqlx"
	"github.com/mattn/go-sqlite3"
	_ "github.com/mattn/go-sqlite3"
	"github.com/pkg/errors"
	"github.com/rs/zerolog"
	sqldblogger "github.com/simukti/sqldb-logger"
	"github.com/simukti/sqldb-logger/logadapter/zerologadapter"
)

type PokemonConnection struct {
	Edges    []*Pokemon `json:"edges"`
	PageInfo PageInfo   `json:"pageInfo"`
}

type PageInfo struct {
	HasNextPage     bool   `json:"hasNextPage"`
	HasPreviousPage bool   `json:"hasPreviousPage"`
	StartCursor     string `json:"startCursor"`
	EndCursor       string `json:"endCursor"`
}

type ForwardPaginationInput struct {
	First int    `json:"first"`
	After string `json:"after"`
}
type BackwordPaginationInput struct {
	Last   int    `json:"last"`
	Before string `json:"before"`
}

type Pokemon struct {
	Index          int64          `db:"index"` // primary key
	No             int64          `db:"図鑑番号"`
	Name           string         `db:"ポケモン名"`
	Type1          string         `db:"タイプ１"`
	Type2          sql.NullString `db:"タイプ２"`
	Ability1       sql.NullString `db:"通常特性１"`
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

type Input struct {
	ForwardPaginationInput
	BackwordPaginationInput
}

func RunEndpoint(ctx context.Context, db *sqlx.DB, input Input) (*PokemonConnection, error) {
	var pageInfo PageInfo

	// validation
	n := 11
	if input.First == 0 && input.Last == 0 {
		if input.After != "" {
			input.First = n
		} else if input.Before != "" {
			input.Last = n
		} else {
			input.First = n
		}
	}

	// TODO: use query builder (for prevent sql injection)
	// https://github.com/graphql/graphql-relay-js/issues/94
	sql := `SELECT * FROM data`

	if input.After != "" {
		fieldAndValue := strings.SplitN(input.After, ":", 2)
		sql = fmt.Sprintf(`%s WHERE %q > %s`, sql, fieldAndValue[0], fieldAndValue[1]) // id:<num>
	}
	if input.First > 0 {
		sql = fmt.Sprintf(`%s ORDER BY "index" asc LIMIT %d`, sql, input.First+1)
		n = input.First + 1
		pageInfo.HasPreviousPage = false
	}

	if input.Before != "" {
		fieldAndValue := strings.SplitN(input.After, ":", 2)
		sql = fmt.Sprintf(`%s WHERE "%q" < %s`, sql, fieldAndValue[0], fieldAndValue[1]) // id:<num>
	}
	if input.Last > 0 {
		sql = fmt.Sprintf(`%s ORDER BY "index" desc LIMIT %d`, sql, input.Last+1)
		n = input.Last + 1
		pageInfo.HasNextPage = false
	}
	sql = sql + ";"

	rows := make([]*Pokemon, 0, n)
	err := db.SelectContext(ctx, &rows, sql)
	if err != nil {
		return nil, errors.Wrap(err, "Select failed")
	}

	if len(rows) == n {
		if input.First > 0 {
			pageInfo.HasNextPage = true
			pageInfo.EndCursor = fmt.Sprintf("index:%d", rows[n-2].Index)
			// If the client is paginating with first/after, then the client may return true if edges prior to after exist, if it can do so efficiently, otherwise may return false.
		}
		if input.Last > 0 {
			pageInfo.HasPreviousPage = true
			pageInfo.StartCursor = fmt.Sprintf("index:%d", rows[n-2].Index)
		}
		rows = rows[:n-1]
	}
	return &PokemonConnection{
		PageInfo: pageInfo,
		Edges:    rows,
	}, nil
}

func initDb(ctx context.Context) (*sqlx.DB, error) {
	logger := zerolog.New(
		zerolog.ConsoleWriter{Out: os.Stdout, NoColor: false},
	)

	// populate log pre-fields here before set to
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

	cursor := ""
	{
		log.Println(`first step`)
		var input Input
		input.First = 5
		r, err := RunEndpoint(ctx, db, input)
		if err != nil {
			return errors.Wrap(err, "query failed")
		}

		rows := r.Edges
		fmt.Printf("%#+v\n", r.PageInfo)
		for i := 0; i < len(rows); i++ {
			fmt.Printf("	%v\n", rows[i])
		}

		cursor = r.PageInfo.EndCursor
	}
	fmt.Println("----------------------------------------")
	{
		log.Printf(`2nd step cursor=%q`, cursor)
		var input Input
		input.First = 5
		input.After = cursor
		r, err := RunEndpoint(ctx, db, input)
		if err != nil {
			return errors.Wrap(err, "query failed")
		}

		rows := r.Edges
		fmt.Printf("%#+v\n", r.PageInfo)
		for i := 0; i < len(rows); i++ {
			fmt.Printf("	%v\n", rows[i])
		}
	}

	return nil
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("!!+%v", err)
	}
}
