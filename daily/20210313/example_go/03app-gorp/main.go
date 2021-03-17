package main

import (
	"context"
	"database/sql"
	"fmt"
	"log"
	"os"

	gorp "github.com/go-gorp/gorp/v3"
	_ "github.com/mattn/go-sqlite3"
	"golang.org/x/xerrors"
)

func main() {
	parseAndRun := func() error {
		return run("sqlite3", ":memory:")
	}
	if err := parseAndRun(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func run(dialect, dbname string) error {
	log.Printf("accessing %s://%s", dialect, dbname)
	ctx := context.Background()

	// initialize the DbMap
	dbmap, err := NewDbMap(ctx, dialect, dbname)
	if err != nil {
		return err
	}
	defer dbmap.Db.Close()

	// // delete any existing rows
	// if err := dbmap.TruncateTables(); err != nil {
	// 	return xerrors.Errorf("TruncateTables failed: %w", err)
	// }

	s, err := NewDbSession(ctx, dbmap, "A")
	if err != nil {
		return err
	}

	s.Users.Insert(ctx,
		&User{Name: ":fake:"},
	)


	users := []*User{
		{Name: "foo"}, // set last inserted id?
		{Name: "bar"},
		{Name: "boo"},
	}
	if err := s.Users.Insert(ctx, users...); err != nil {
		return fmt.Errorf("insert user %w", err)
	}

	fmt.Printf("%#+v\n", s.Tenant)
	for _, user := range users {
		fmt.Printf("	- %#+v\n", *user)
	}
	count, err := dbmap.SelectInt("select count(*) from user")
	if err != nil {
		return fmt.Errorf("select count(*) failed: %w", err)
	}
	fmt.Println("@", count)
	return nil
}

func NewDbMap(
	ctx context.Context,
	dialect, dbname string,
) (*gorp.DbMap, error) {
	db, err := sql.Open(dialect, dbname)
	if err != nil {
		return nil, xerrors.Errorf("sql.Open failed: %w", err)
	}

	// construct a gorp DbMap
	dbmap := &gorp.DbMap{Db: db, Dialect: gorp.SqliteDialect{}}

	dbmap.AddTableWithName(Tenant{}, "tenant").SetKeys(true, "ID")
	dbmap.AddTableWithName(User{}, "user").SetKeys(true, "ID")

	dbmap.TraceOn("[gorp]", log.New(os.Stdout, "myapp:", log.Lmicroseconds))
	// dbmap.TraceOff()

	// create the table. in a production system you'd generally
	// use a migration tool, or create the tables via scripts
	err = dbmap.CreateTablesIfNotExists()
	if err != nil {
		return nil, xerrors.Errorf("Create tables failed: %w", err)
	}
	return dbmap, nil
}

type DbSession struct {
	*gorp.DbMap
	Tenant

	Users *Users
}
type Users DbSession

func (s *Users) New(ob *User) *User { // before create?
	// cannot use gorp's hook, because injecting *tenant.id*
	ob.TenantID = s.Tenant.ID
	return ob
}
func (s *Users) Insert(ctx context.Context, obs ...*User) error {
	values := make([]interface{}, len(obs))
	for i, ob := range obs {
		obs[i] = s.New(ob)
		values[i] = obs[i]
	}
	if err := s.DbMap.Insert(values...); err != nil {
		return fmt.Errorf("insert values -- %w", err)
	}
	return nil
}

func NewDbSession(ctx context.Context, m *gorp.DbMap, name string) (*DbSession, error) {
	root := Tenant{Name: name}
	if err := m.Insert(&root); err != nil {
		return nil, fmt.Errorf("Insert tenant failed %w", err)
	}

	s := &DbSession{
		DbMap:  m,
		Tenant: root,
	}
	s.Users = (*Users)(s)
	return s, nil
}

type Tenant struct {
	ID   int64  `db:"id,primarykey,autoincrement"`
	Name string `db:"name,size:255"`
}
type User struct {
	ID       int64  `db:"id,primarykey,autoincrement"`
	TenantID int64  `db:"tenant_id"` // foreignkey:tenant.id
	Name     string `db:"name,size:255"`
}
