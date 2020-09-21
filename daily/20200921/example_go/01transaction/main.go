package main

import (
	"context"
	"database/sql"
	"flag"
	"log"

	_ "github.com/mattn/go-sqlite3"
	"golang.org/x/xerrors"
)

// https://saitodev.co/article/Go%E8%A8%80%E8%AA%9E%E3%81%A8SQLite%E3%81%A7%E3%83%88%E3%83%A9%E3%83%B3%E3%82%B6%E3%82%AF%E3%82%B7%E3%83%A7%E3%83%B3/

func main() {
	dbname := flag.String("db", "", "dbname")
	flag.Parse()
	if err := run(*dbname); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func run(dbname string) error {
	ctx := context.Background()

	db, err := sql.Open("sqlite3", dbname)
	if err != nil {
		return xerrors.Errorf("open %w", err)
	}

	tx, err := db.Begin()
	if err != nil {
		return xerrors.Errorf("begin %w", err)
	}
	defer tx.Commit()

	/** ここからuser_idが1のpriceから1000を引いて更新する処理 **/
	var uid int
	var price int
	{
		stmt, err := tx.PrepareContext(ctx, "UPDATE calc set price = ? WHERE id = ?")
		if err != nil {
			return xerrors.Errorf("prepare %w", err)
		}

		rows, err := tx.QueryContext(ctx, "SELECT id, price from calc WHERE user_id=?", 1)
		if err != nil {
			return xerrors.Errorf("query %w", err)
		}

		defer rows.Close()
		for rows.Next() {
			err = rows.Scan(&uid, &price)
			if err != nil {
				return xerrors.Errorf("scan %w", err)
			}

			log.Printf(" got: price %d (user_id=%d)\n", price, 1)
			newPrice := price - 1000
			log.Printf("exec: price %d -> %d\n", price, newPrice)

			_, err = stmt.ExecContext(ctx, newPrice, uid)
			if err != nil {
				return xerrors.Errorf("update %w", err)
			}
		}
	}
	/** ここまでuser_idが1のpriceから1000を引いて更新する処理 **/

	/** ここからuser_idが2のpriceから1000を足して更新する処理 **/
	{
		rows, err := tx.QueryContext(ctx, "SELECT id, price FROM calc WHERE user_id=2;")
		if err != nil {
			return xerrors.Errorf("query %w", err)
		}

		defer rows.Close()

		for rows.Next() {
			err = rows.Scan(&uid, &price)
			if err != nil {
				return xerrors.Errorf("scan2 %w", err)
			}

			log.Printf(" got: price %d (user_id=%d)\n", price, 2)
			newPrice := price + 1000
			log.Printf("exec: price %d -> %d\n", price, newPrice)
			_, err = tx.ExecContext(ctx, "UPDATE calc SET price = ? WHERE id=?", newPrice, uid)
			if err != nil {
				return xerrors.Errorf("exec %w", err)
			}
		}
	}
	/** ここまでuser_idが2のpriceから1000を足して更新する処理 **/
	return nil
}
