package main

import (
	"context"
	"database/sql"
	"fmt"
	"log"

	"contrib.go.opencensus.io/integrations/ocsql"
	_ "github.com/mattn/go-sqlite3"
	"go.opencensus.io/trace"
	"golang.org/x/xerrors"
)

type customTraceExporter struct{}

func (ce *customTraceExporter) ExportSpan(sd *trace.SpanData) {
	// sd.Attributes
	fmt.Printf("Name: %s\nTraceID: %x\nAttributes: %#+v\n\n",
		sd.Name, sd.TraceID, sd.Attributes)
}

func main() {
	{
		trace.ApplyConfig(trace.Config{DefaultSampler: trace.AlwaysSample()})

		// Please remember to register your exporter
		// so that it can receive exported spanData.
		trace.RegisterExporter(new(customTraceExporter))
	}

	if err := run(); err != nil {
		log.Fatalf("!!%+v", err)
	}
}

func run() error {
	var (
		driverName string
		err        error
		db         *sql.DB
	)

	// Register default views.
	ocsql.RegisterAllViews()

	// Register our ocsql wrapper for the provided SQLite3 driver.
	driverName, err = ocsql.Register(
		"sqlite3",
		ocsql.WithAllTraceOptions(),
		ocsql.WithInstanceName("resources"),
	)
	if err != nil {
		return xerrors.Errorf("ocsql register %w", err)
	}

	// Connect to a SQLite3 database using the ocsql driver wrapper.
	db, err = sql.Open(driverName, "resource.db")
	if err != nil {
		return xerrors.Errorf("sql open %w", err)
	}

	ctx := context.Background()
	if err := do(ctx, db); err != nil {
		return err
	}
	fmt.Println("----------------------------------------")
	if err := do(ctx, db); err != nil {
		return err
	}
	return nil
}

func do(ctx context.Context, db *sql.DB) error {
	tx, err := db.Begin()
	if err != nil {
		return xerrors.Errorf("tx begin %w", err)
	}
	defer tx.Commit() // todo: rollback

	rows, err := db.QueryContext(ctx, "select * from users")
	if err != nil {
		return xerrors.Errorf("sql query %w", err)
	}

	i := 0
	for rows.Next() {
		var val struct {
			ID   int
			Name string
		}
		err = rows.Scan(&val.ID, &val.Name)
		if err != nil {
			return xerrors.Errorf("scan %d %w", i, err)
		}
		fmt.Println(val)
		i++
	}
	return nil
}
