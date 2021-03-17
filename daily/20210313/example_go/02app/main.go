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
	"github.com/rs/zerolog"
	sqldblogger "github.com/simukti/sqldb-logger"
	"github.com/simukti/sqldb-logger/logadapter/zerologadapter"
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
	m, err := NewDBManager(ctx, dialect, dbname)
	if err != nil {
		return fmt.Errorf("new db manager -- %w", err)
	}
	if err := setupDB(ctx, m); err != nil {
		return err
	}

	s, err := NewDBSession(ctx, m, "A")
	if err != nil {
		return fmt.Errorf("new db session -- %w", err)
	}

	s.Users.Insert(ctx,
		User{Name: ":fake:"},
	)

	users, err := s.Users.Insert(ctx,
		User{Name: "foo"},
		User{Name: "bar"},
		User{Name: "boo"},
	)
	if err != nil {
		return fmt.Errorf("Insert failed %w", err)
	}

	fmt.Printf("%#+v\n", s.Tenant)
	for _, user := range users {
		fmt.Printf("	- %#+v\n", user)
	}
	return nil
}

type DBManager struct {
	DB           *sqlx.DB
	Transcriptor *Transcriptor

	InfoMap map[string]TableInfo
	mapper  *reflectx.Mapper
	setups  []SetupAction
}

func newDBManager(
	ctx context.Context,
	dialect, dbname string,
) (*DBManager, error) {
	mapper := reflectx.NewMapper("db")
	var db *sql.DB
	db, err := sql.Open(dialect, dbname)
	if err != nil {
		return nil, fmt.Errorf("sql.Open failed %w", err)
	}

	logger := zerolog.New(
		zerolog.ConsoleWriter{Out: os.Stdout, NoColor: false},
	)
	// populate log pre-fields here before set to
	db = sqldblogger.OpenDriver(
		dbname,
		db.Driver(),
		zerologadapter.New(logger),
		// optional config...
	)

	sqlxdb := sqlx.NewDb(db, dialect)
	if err = db.PingContext(ctx); err != nil {
		return nil, fmt.Errorf("ping failed %w", err)
	}

	if _, err := sqlxdb.ExecContext(ctx, "PRAGMA foreign_keys = ON;"); err != nil {
		return nil, fmt.Errorf("exec failed, for activate foreign key %w", err)
	}

	return &DBManager{
		DB:           sqlxdb,
		Transcriptor: &Transcriptor{mapper: mapper},

		mapper:  mapper,
		InfoMap: map[string]TableInfo{},
	}, nil
}

func NewDBManager(
	ctx context.Context,
	dialect, dbname string,
) (*DBManager, error) {
	m, err := newDBManager(ctx, dialect, dbname)
	if err != nil {
		return m, fmt.Errorf("new db manager -- %w", err)
	}
	return m, nil
}

func setupDB(ctx context.Context, m *DBManager) error {
	// create table
	if err := m.RegisterTable(
		UserTable,
		TenantTable,
	); err != nil {
		return fmt.Errorf("pre setup db -- %w", err)
	}
	if err := m.SetupDatabase(ctx); err != nil {
		return fmt.Errorf("setup db -- %w", err)
	}
	return nil
}

func (m *DBManager) RegisterTable(infoList ...TableInfo) error {
	for _, info := range infoList {
		info := info
		name := info.Name
		if another, exists := m.InfoMap[name]; exists {
			return fmt.Errorf("table %q is already registered, as %T", name, another.Def)
		}
		m.InfoMap[name] = info
		m.setups = append(m.setups, SetupAction{
			Name: fmt.Sprintf("table:%s", name),
			Callback: func(ctx context.Context, m *DBManager) error {
				_, err := m.DB.ExecContext(ctx, m.Transcriptor.CreateTableStmt(info))
				if err != nil {
					return fmt.Errorf("failed when create table %q", name)
				}
				return err
			},
		})
	}
	return nil
}

type SetupAction struct {
	Name     string
	Callback func(ctx context.Context, m *DBManager) error
}

func (m *DBManager) SetupDatabase(ctx context.Context) error {
	for _, action := range m.setups {
		if err := action.Callback(ctx, m); err != nil {
			if err != nil {
				return fmt.Errorf("setup error in %q", action.Name)
			}
		}
	}
	return nil
}

type Transcriptor struct {
	mapper *reflectx.Mapper
}

func (t *Transcriptor) CreateTableStmt(infoList ...TableInfo) string {
	// https://sqlite.org/lang_createtable.html
	mapper := t.mapper
	type fk struct {
		local  string
		remote string
	}

	w := bytes.NewBufferString("")
	for _, info := range infoList {
		// log.Println("-- %d: for %s table", i, info.Name)
		smap := mapper.TypeMap(reflect.TypeOf(info.Def))

		var foreignKeys []fk
		w.WriteString("CREATE TABLE ")
		w.WriteString(info.Name)
		w.WriteString(" (\n")
		for _, index := range smap.Index {
			var args []string
			args = append(args, GuessColumnType(index))
			if _, ok := index.Options["primarykey"]; ok {
				args = append(args, "PRIMARY KEY")
			}
			if _, ok := index.Options["autoincrement"]; ok {
				args = append(args, "AUTOINCREMENT")
			}
			fmt.Fprintf(w, "  %s %s NOT NULL", index.Name, strings.Join(args, " "))
			w.WriteString(",\n")

			if v, ok := index.Options["foreignkey"]; ok {
				foreignKeys = append(foreignKeys, fk{local: index.Name, remote: v})
			}
		}
		for _, fk := range foreignKeys {
			rparts := strings.SplitN(fk.remote, ".", 2)                                          // <table name>.<column name>
			fmt.Fprintf(w, "FOREIGN KEY (%s) REFERENCES %s(%s)", fk.local, rparts[0], rparts[1]) // TODO: ON DELETE/UPDATE
			w.WriteString(",\n")
		}
		w.Truncate(w.Len() - 2) // ,\n
		w.WriteString("\n")
		w.WriteString(");")
	}
	return w.String()
}

// TODO: cache

func (t *Transcriptor) InsertStmt(info TableInfo) string {
	mapper := t.mapper
	tablename := info.Name
	ob := info.Def

	smap := mapper.TypeMap(reflect.TypeOf(ob))
	w := bytes.NewBufferString("")

	fmt.Fprintf(w, "INSERT INTO %s (", tablename)
	{
		skiped := 0
		for _, index := range smap.Index {
			if _, isPrimaryKey := index.Options["primarykey"]; isPrimaryKey {
				skiped++
				continue
			}
			w.WriteString(index.Name)
			w.WriteString(", ")
		}
		if len(smap.Index)-skiped > 0 {
			w.Truncate(w.Len() - 2) // UnwriteString(", ")
		}
	}
	w.WriteString(") VALUES (")
	{
		skiped := 0
		for _, index := range smap.Index {
			if _, isPrimaryKey := index.Options["primarykey"]; isPrimaryKey {
				skiped++
				continue
			}
			w.WriteString(":")
			w.WriteString(index.Name)
			w.WriteString(", ")
		}
		if len(smap.Index)-skiped > 0 {
			w.Truncate(w.Len() - 2) // UnwriteString(", ")
		}
	}
	w.WriteString(")") // ;をつけると1つだけしか挿入されない
	return w.String()
}

// TODO: dry-run
// NEWLINE, writer, infoList

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

type DBSession struct {
	*DBManager
	Tenant

	Users *Users
}

type Users DBSession

func (s *Users) New(ob *User) *User { // before create?
	ob.TenantID = s.Tenant.ID
	return ob
}
func (s *Users) Insert(ctx context.Context, obs ...User) ([]User, error) {
	for i, ob := range obs {
		obs[i] = *s.New(&ob)
	}

	r, err := s.DBManager.DB.NamedExecContext(
		ctx,
		s.DBManager.Transcriptor.InsertStmt(UserTable), // TODO: use insert into (...) values (...) returning <pk>
		obs,
	)
	if err != nil {
		return nil, fmt.Errorf("insert values -- %w", err)
	}

	// TODO: this is work-around
	if len(obs) == 1 {
		id, err := r.LastInsertId()
		if err != nil {
			return nil, fmt.Errorf("refetch values1 -- %w", err)
		}
		obs[0].ID = id
		return obs, nil
	}
	c, err := r.RowsAffected()
	if err != nil {
		return nil, fmt.Errorf("refetch valuesinfo -- %w", err)
	}
	// assert c == len(obs)
	var result []User
	if err := s.DBManager.DB.SelectContext(ctx, &result, "select * from user where tenant_id=? order by id desc limit ?", s.Tenant.ID, c); err != nil {
		return nil, fmt.Errorf("refetch values -- %w", err)
	}
	return result, nil
}

func NewDBSession(ctx context.Context, m *DBManager, name string) (*DBSession, error) {
	root := Tenant{Name: name}
	r, err := m.DB.NamedExecContext(
		ctx,
		m.Transcriptor.InsertStmt(TenantTable),
		&root,
	)
	if err != nil {
		return nil, fmt.Errorf("Insert tenant failed %w", err)
	}
	lastInsertID, err := r.LastInsertId()
	if err != nil {
		return nil, fmt.Errorf("Insert tenant failed %w", err)
	}
	root.ID = lastInsertID

	s := &DBSession{
		DBManager: m,
		Tenant:    root,
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
	TenantID int64  `db:"tenant_id,foreignkey=tenant.id"`
	Name     string `db:"name,size:255"`
}

type TableInfo struct {
	Name string
	Def  interface{}
}

var (
	UserTable   = TableInfo{Name: "user", Def: User{}}
	TenantTable = TableInfo{Name: "tenant", Def: Tenant{}}
)
