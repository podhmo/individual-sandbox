package main

import (
	"context"
	"database/sql"
	"encoding/base64"
	"encoding/json"
	"fmt"
	"log"
	"os"
	"time"

	"github.com/jmoiron/sqlx"
	"github.com/mattn/go-sqlite3"
	_ "github.com/mattn/go-sqlite3"
	"github.com/rs/zerolog"
	sqldblogger "github.com/simukti/sqldb-logger"
	"github.com/simukti/sqldb-logger/logadapter/zerologadapter"
	"golang.org/x/xerrors"
)

type Cursor struct {
	CreatedAt time.Time  `json:"createdAt"`
	Items     []SortItem `json:"items"`
}
type cursor Cursor

func (cur *Cursor) UnmarshalText(b []byte) error {
	if len(b) == 0 {
		return nil
	}

	buf := make([]byte, 0, 256)
	n, err := base64.StdEncoding.Decode(buf, b)
	if err != nil {
		return err
	}
	buf = buf[:n]

	var internal cursor
	if err := json.Unmarshal(buf, &internal); err != nil {
		return err
	}
	cur.Items = internal.Items
	return nil
}
func (cur Cursor) MarshalText() ([]byte, error) {
	if len(cur.Items) == 0 {
		return []byte(""), nil
	}

	internal := (*cursor)(&cur)
	b, err := json.Marshal(internal)
	if err != nil {
		return nil, err
	}

	enc := base64.StdEncoding
	buf := make([]byte, base64.StdEncoding.EncodedLen(len(b)))
	enc.Encode(buf, b)
	return buf, nil
}

type SortItem struct {
	Key  SortKey     `json:"k"`
	Last interface{} `json:"v"`
}
type SortKey struct {
	Field string
	Desc  bool
}

func (sk *SortKey) UnmarshalText(b []byte) error {
	if len(b) == 0 {
		return nil
	}

	if b[0] == '-' {
		sk.Desc = true
		sk.Field = string(b[1:])
	} else {
		sk.Field = string(b)
	}
	return nil
}
func (sk SortKey) MarshalText() ([]byte, error) {
	if sk.Desc {
		return []byte("-" + sk.Field), nil
	}
	return []byte(sk.Field), nil
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

type PokemonResponse struct {
	Rows   []*Pokemon
	Cursor *Cursor
}

func RunEndpoint(ctx context.Context, db *sqlx.DB) (*PokemonResponse, error) {
	n := 11
	rows := make([]*Pokemon, 0, n)

	sql := fmt.Sprintf(`SELECT * FROM data ORDER BY "こうげき" desc, "index" LIMIT %d`, n)
	if err := db.SelectContext(ctx, &rows, sql); err != nil {
		return nil, xerrors.Errorf("Select failed -- %w", err)
	}
	return &PokemonResponse{
		Rows: rows,
		Cursor: &Cursor{Items: []SortItem{
			{Key: SortKey{Field: "攻撃", Desc: true}, Last: rows[n-2].Attack},
			{Key: SortKey{Field: "index"}, Last: rows[n-2].Index},
		}},
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
		return nil, xerrors.Errorf("ping failed -- %w", err)

	}
	return sqlxdb, nil
}

func run() error {
	ctx := context.Background()

	db, err := initDb(ctx)
	if err != nil {
		return err
	}

	{
		log.Println(`first step`)
		r, err := RunEndpoint(ctx, db)
		if err != nil {
			return xerrors.Errorf("query failed -- %w", err)
		}

		enc := json.NewEncoder(os.Stdout)
		enc.SetIndent("", "  ")
		if err := enc.Encode(r.Cursor); err != nil {
			return xerrors.Errorf("encode -- %w", err)
		}
		rows := r.Rows
		for i := 0; i < len(rows); i++ {
			fmt.Printf("%v\n", rows[i])
		}
	}
	fmt.Println("----------------------------------------")
	return nil
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("!!+%v", err)
	}
}
