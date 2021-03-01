package main

import (
	"bytes"
	"context"
	"database/sql"
	"fmt"
	"log"
	"os"
	"reflect"
	"strings"

	"github.com/jmoiron/sqlx"
	"github.com/jmoiron/sqlx/reflectx"
	_ "github.com/mattn/go-sqlite3"
	"github.com/pkg/errors"
	"github.com/rs/zerolog"
	sqldblogger "github.com/simukti/sqldb-logger"
	"github.com/simukti/sqldb-logger/logadapter/zerologadapter"
)

// https://docs.mongodb.com/manual/tutorial/query-documents/
// https://sqlite.org/datatype3.html
// ../../../20200527/example_tinyorm/02sqlx/main.go

type Inventory struct {
	ID      int64   `db:"id,primarykey,autoincrement"`
	Item    string  `db:"item,size:255"`
	Qty     int64   `db:"qty"`
	SizeW   float64 `db:"sizeW"`
	SizeH   float64 `db:"sizeH"`
	SizeUOM string  `db:"sizeUOM,size:15"`
	Status  string  `db:"status,size:15"` // enum?
}

func initDb(ctx context.Context, mapper *reflectx.Mapper) (*sqlx.DB, error) {
	var db *sql.DB
	db, err := sql.Open("sqlite3", ":memory:")
	if err != nil {
		return nil, errors.Wrap(err, "sql.Open failed")
	}

	logger := zerolog.New(
		zerolog.ConsoleWriter{Out: os.Stdout, NoColor: false},
	)
	// populate log pre-fields here before set to
	db = sqldblogger.OpenDriver(
		":memory:",
		db.Driver(),
		zerologadapter.New(logger),
		// optional config...
	)

	sqlxdb := sqlx.NewDb(db, "sqlite3")
	if err = db.PingContext(ctx); err != nil {
		return nil, errors.Wrap(err, "ping failed")

	}

	// create table
	sqlxdb.MustExecContext(ctx, BuildCreateDbStmt(mapper, TableInfo{"inventory", Inventory{}}))
	return sqlxdb, nil
}

type TableInfo struct {
	Name string
	Def  interface{}
}

// TODO: dry-run
// NEWLINE, writer, infoList

func BuildCreateDbStmt(mapper *reflectx.Mapper, infoList ...TableInfo) string {
	w := bytes.NewBufferString("")
	for _, info := range infoList {
		// log.Println("-- %d: for %s table", i, info.Name)
		smap := mapper.TypeMap(reflect.TypeOf(info.Def))
		w.WriteString("CREATE TABLE ")
		w.WriteString(info.Name)
		w.WriteString(" (\n")
		for i, index := range smap.Index {
			var args []string
			args = append(args, GuessColumnType(index))
			if _, ok := index.Options["primarykey"]; ok {
				args = append(args, "PRIMARY KEY")
			}
			if _, ok := index.Options["autoincrement"]; ok {
				args = append(args, "AUTOINCREMENT")
			}
			fmt.Fprintf(w, "  %s %s", index.Name, strings.Join(args, " "))
			if i < len(smap.Index)-1 {
				w.WriteString(", ")
			}
			w.WriteString("\n")
		}
		w.WriteString(");")
	}
	return w.String()
}

func GuessColumnType(finfo *reflectx.FieldInfo) string {
	switch finfo.Zero.Type() {
	case reflect.TypeOf(int64(0)):
		return "INTEGER"
	case reflect.TypeOf(float64(0.0)):
		return "REAL"
	case reflect.TypeOf(""):
		return "TEXT"
	default:
		return "UNKNOWN" // todo: return error?
	}
}

func BuildInsertRecordStmt(mapper *reflectx.Mapper, tablename string, ob interface{}) string {
	smap := mapper.TypeMap(reflect.TypeOf(ob))

	w := bytes.NewBufferString("")
	fmt.Fprintf(w, "INSERT INTO %s (", tablename)
	for i, index := range smap.Index {
		if _, isPrimaryKey := index.Options["primarykey"]; isPrimaryKey {
			continue
		}
		w.WriteString(index.Name)
		if i < len(smap.Index)-1 {
			w.WriteString(", ")
		}
	}
	fmt.Fprintf(w, ") VALUES(")
	for i, index := range smap.Index {
		if _, isPrimaryKey := index.Options["primarykey"]; isPrimaryKey {
			continue
		}
		w.WriteString(":")
		w.WriteString(index.Name)
		if i < len(smap.Index)-1 {
			w.WriteString(", ")
		}
	}
	fmt.Fprintf(w, ")") // ;をつけると1つだけしか挿入されない
	return w.String()
}

func run() error {
	mapper := reflectx.NewMapper("db")

	ctx := context.Background()
	db, err := initDb(ctx, mapper)
	if err != nil {
		return err
	}

	inventories := []Inventory{
		{Item: "journal", Qty: 25, SizeH: 14, SizeW: 21, SizeUOM: "cm", Status: "A"},
		{Item: "notebook", Qty: 50, SizeH: 8.5, SizeW: 11, SizeUOM: "in", Status: "A"},
		{Item: "paper", Qty: 100, SizeH: 8.5, SizeW: 11, SizeUOM: "in", Status: "D"},
		{Item: "planner", Qty: 75, SizeH: 22.85, SizeW: 30, SizeUOM: "cm", Status: "D"},
		{Item: "postcard", Qty: 45, SizeH: 10, SizeW: 15.25, SizeUOM: "cm", Status: "A"},
		{Item: "unknown", Qty: 99, SizeH: 9, SizeW: 9, SizeUOM: "cm", Status: "X"},
	}

	// insert
	{
		r, err := db.NamedExecContext(
			ctx,
			BuildInsertRecordStmt(mapper, "inventory", Inventory{}),
			inventories,
		)

		if err != nil {
			return errors.Wrap(err, "Insert failed")
		}
		id, err := r.LastInsertId()
		if err != nil {
			return errors.Wrap(err, "get last Inserted ID failed")
		}
		fmt.Println("@", id)
	}
	return nil
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("!!+%v", err)
	}
}
