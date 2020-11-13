package main

import (
	"flag"
	"log"
	"m/database"

	_ "github.com/mattn/go-sqlite3"
)

func main() {
	dburlP := flag.String("dburl", "", "")
	flag.Parse()
	if *dburlP == "" {
		*dburlP = ":memory:"
	}

	run := func() error {
		// initialize the DbMap
		db, err, close := database.New(*dburlP)
		if err != nil {
			return err
		}
		defer close()
		return Run(db)
	}
	if err := run(); err != nil {
		log.Printf("!! %+v", err)
	}
}

func Run(db *database.DB) error {
	defer log.Println("Done!")

	// create two posts
	p1 := db.Post.New("Go 1.1 released!", "Lorem ipsum lorem ipsum")
	p2 := db.Post.New("Go 1.2 released!", "Lorem ipsum lorem ipsum")

	if err := db.Post.Create(p1, p2); err != nil {
		return err
	}

	count, err := db.Post.Count()
	if err != nil {
		return err
	}
	log.Println("Rows after inserting:", count)

	p2.Title = "Go 1.2 is better than ever"
	count, err = db.Post.Save(p2)
	if err != nil {
		return err
	}
	log.Println("Rows updated:", count)

	p2, err = db.Post.SelectOne(p2.Id)
	if err != nil {
		return err
	}
	log.Println("p2 row:", p2)

	// fetch all rows
	posts, err := db.Post.SelectAll()
	if err != nil {
		return err
	}
	log.Println("All rows:")
	for x, p := range posts {
		log.Printf("    %d: %v\n", x, p)
	}

	// delete row by PK
	r, err := db.Post.Delete(p1.Id)
	if err != nil {
		return err
	}
	log.Println("Rows deleted:", r)

	// confirm count is zero
	count, err = db.Post.Count()
	if err != nil {
		return err
	}
	log.Println("Row count - should be zero:", count)
	return nil
}
