package main

import (
	"context"
	"database/sql"
	"fmt"
	"log"
	"net/http"
	_ "net/http/pprof"
	"os"
	"sync"

	"github.com/go-chi/chi"
	"github.com/go-chi/chi/middleware"
	"github.com/go-chi/render"
	_ "github.com/mattn/go-sqlite3"
	"golang.org/x/xerrors"
)

type Config struct {
	DBName string
	Addr   string
}

type AppSession struct {
	Config Config

	db *sql.DB
	tx *sql.Tx

	mu        sync.Mutex // use RWMutex
	teardowns []func()
}

func (as *AppSession) Close() {
	as.mu.Lock()
	defer as.mu.Unlock()
	for i := len(as.teardowns) - 1; i > 0; i-- {
		fmt.Println("xxx", i)
		as.teardowns[i]()
	}
	as.teardowns = nil
}

func (as *AppSession) NewDB() (*sql.DB, error) {
	as.mu.Lock()
	defer as.mu.Unlock()

	if as.db != nil {
		return as.db, nil
	}

	db, err := sql.Open("sqlite3", as.Config.DBName)
	if err != nil {
		return nil, xerrors.Errorf("open %w", err)
	}
	as.db = db
	as.teardowns = append(as.teardowns, func() {
		// todo: correct error handling
		if err := as.db.Close(); err != nil {
			log.Println(err)
		}
	})
	return db, nil
}

func (as *AppSession) NewTx() (*sql.Tx, error) {
	db, err := as.NewDB()
	if err != nil {
		return nil, xerrors.Errorf("tx new db %w", err)
	}

	as.mu.Lock()
	defer as.mu.Unlock()

	if as.tx != nil {
		return as.tx, nil
	}

	tx, err := db.Begin()
	if err != nil {
		return nil, xerrors.Errorf("tx begin %w", err)
	}
	as.tx = tx
	as.teardowns = append(as.teardowns, func() {
		// todo: rollback handling
		tx.Commit()
	})
	return tx, nil
}

type Skill struct {
	ID   int
	Name string
}
type User struct {
	ID   int
	Name string
}

func main() {
	addr := os.Getenv("ADDR")
	if addr == "" {
		addr = ":8888"
	}
	config := Config{
		DBName: "sample.db",
		Addr:   addr,
	}

	paddr := os.Getenv("PPROF_ADDR")
	if paddr == "" {
		paddr = "localhost:44444"
	}
	go func() {
		log.Println(http.ListenAndServe(paddr, nil))
	}()

	r := chi.NewRouter()
	r.Use(middleware.Logger)

	r.Get("/", func(w http.ResponseWriter, r *http.Request) {
		ctx := r.Context()

		as := &AppSession{Config: config}
		defer as.Close()

		userID := 1 // xxx
		user, err := GetUser(ctx, as, userID)
		if err != nil {
			// todo: handling not found
			log.Printf("! %+v", err)
			render.Status(r, 500)
			render.JSON(w, r, map[string]interface{}{"message": err.Error()})
		}
		skills, err := GetSkills(ctx, as, userID)
		if err != nil {
			log.Printf("! %+v", err)
			render.Status(r, 500)
			render.JSON(w, r, map[string]interface{}{"message": err.Error()})
		}
		render.JSON(w, r, map[string]interface{}{
			"user":   user,
			"skills": skills,
		})
	})

	log.Printf("listen %v", config.Addr)
	if err := http.ListenAndServe(config.Addr, r); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

type TxFactory interface {
	NewTx() (*sql.Tx, error)
}

type deps interface {
	TxFactory
}

func GetUser(ctx context.Context, deps deps, userID int) (*User, error) {
	tx, err := deps.NewTx()
	if err != nil {
		return nil, xerrors.Errorf("new tx %w", err)
	}

	rows, err := tx.QueryContext(ctx, "SELECT id, name from users WHERE id=?", userID)
	if err != nil {
		return nil, xerrors.Errorf("query %w", err)
	}
	defer rows.Close()

	i := 0
	for rows.Next() {
		var user User
		err = rows.Scan(&user.ID, &user.Name)
		if err != nil {
			return nil, xerrors.Errorf("scan %d %w", i, err)
		}
		return &user, nil
	}
	return nil, fmt.Errorf("not found") // xxx
}

func GetSkills(ctx context.Context, deps deps, userID int) ([]Skill, error) {
	tx, err := deps.NewTx()
	if err != nil {
		return nil, xerrors.Errorf("new tx %w", err)
	}

	rows, err := tx.QueryContext(ctx, "SELECT id, name from skills WHERE user_id=?", userID)
	if err != nil {
		return nil, xerrors.Errorf("query %w", err)
	}
	defer rows.Close()

	var skills []Skill
	i := 0
	for rows.Next() {
		var skill Skill
		err = rows.Scan(&skill.ID, &skill.Name)
		if err != nil {
			return nil, xerrors.Errorf("scan %d %w", i, err)
		}

		i++
		skills = append(skills, skill)
	}
	return skills, nil
}
