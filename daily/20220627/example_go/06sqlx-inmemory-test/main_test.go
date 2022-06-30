package main

import (
	"context"
	"fmt"
	"testing"

	"github.com/google/go-cmp/cmp"
	"github.com/google/go-cmp/cmp/cmpopts"
	"github.com/jmoiron/sqlx"
	_ "modernc.org/sqlite"
)

type User struct {
	ID   int64  `db:"id,primarykey,autoincrement"`
	Name string `db:"name"`
	Age  int    `db:"age"`

	// Password string `db:"password"` // password type
	// TODO: null string, foreign key
}

type Option func(context.Context, *sqlx.DB) error

func newDB(ctx context.Context, t *testing.T, options ...Option) (*sqlx.DB, func()) {
	pool, err := sqlx.ConnectContext(ctx, "sqlite", ":memory:")
	if err != nil {
		t.Fatalf("connect db: %+v", err)
	}

	for _, opt := range options {
		if err := opt(ctx, pool); err != nil {
			t.Errorf("connect option: %+v", err)
		}
	}
	return pool, func() {
		if err := pool.Close(); err != nil {
			t.Fatalf("close db: %+v", err)
		}
	}
}

func withUsers(users []User) Option {
	return func(ctx context.Context, db *sqlx.DB) error {
		// create table
		stmt := `
	CREATE TABLE User (
		id INTEGER PRIMARY KEY AUTOINCREMENT,
		name TEXT NOT NULL,
		age INTEGER NOT NULL DEFAULT 0
	  );
	  `
		_, err := db.ExecContext(ctx, stmt)
		if err != nil {
			return fmt.Errorf("failed in create table User: %w", err)
		}

		// TODO: bulk insert
		for _, u := range users {
			stmt := `INSERT INTO User (name, age) VALUES(?, ?)`
			_, err := db.ExecContext(ctx, stmt, u.Name, u.Age)
			if err != nil {
				return fmt.Errorf("insert data: %w", err)
			}
		}
		return nil
	}
}

func TestSelectOne(t *testing.T) {
	ctx := context.Background()

	users := []User{
		{Name: "foo", Age: 20},
		{Name: "bar", Age: 20},
	}
	db, teardown := newDB(ctx, t, withUsers(users))
	defer teardown()

	var got User
	want := users[0]

	stmt := "SELECT * FROM User where name=?"
	if err := db.GetContext(ctx, &got, stmt, want.Name); err != nil {
		t.Fatalf("unexpected error: %+v", err)
	}

	if diff := cmp.Diff(want, got, cmpopts.IgnoreFields(User{}, "ID")); diff != "" {
		t.Errorf("GetContext() mismatch (-want +got):\n%s", diff)
	}
}

func TestSelectMany(t *testing.T) {
	ctx := context.Background()

	users := []User{
		{Name: "foo", Age: 20},
		{Name: "bar", Age: 20},
	}
	db, teardown := newDB(ctx, t, withUsers(users))
	defer teardown()

	var got []User

	stmt := "SELECT * FROM User where age=? ORDER BY id"
	if err := db.SelectContext(ctx, &got, stmt, users[0].Age); err != nil {
		t.Fatalf("unexpected error: %+v", err)
	}

	want := users
	if want, got := len(want), len(got); want != got {
		t.Errorf("SelectContext() mismatch len(want)=%d != len(got)=%d", want, got)
	}
	type ref struct{ Value []User }
	if diff := cmp.Diff(ref{want}, ref{got}, cmpopts.IgnoreFields(User{}, "ID")); diff != "" {
		t.Errorf("SelectContext() mismatch (-want +got):\n%s", diff)
	}
}

func TestCount(t *testing.T) {
	ctx := context.Background()

	users := []User{
		{Name: "foo", Age: 20},
		{Name: "bar", Age: 20},
	}
	db, teardown := newDB(ctx, t, withUsers(users))
	defer teardown()

	var got int
	stmt := "SELECT COUNT(*) FROM User;"
	if err := db.GetContext(ctx, &got, stmt); err != nil {
		t.Fatalf("unexpected error: %+v", err)
	}

	if want := len(users); want != got {
		t.Errorf("GetContext() mismatch want=%d != got=%d", want, got)
	}
}

func TestIn(t *testing.T) {
	ctx := context.Background()

	users := []User{
		{Name: "foo", Age: 20},
		{Name: "bar", Age: 20},
		{Name: "boo", Age: 21},
	}
	db, teardown := newDB(ctx, t, withUsers(users))
	defer teardown()

	cases := []struct {
		stmt string
		args map[string]interface{}
		want []User
	}{
		{
			want: []User{{Name: "bar", Age: 20}},
			stmt: "SELECT * FROM User WHERE age=:age AND id IN (:ids);",
			args: map[string]interface{}{
				"age": 20,
				"ids": []int{2, 3},
			}},
		{
			want: []User{{Name: "bar", Age: 20}},
			stmt: "SELECT * FROM User WHERE id IN (:ids) AND age=:age;",
			args: map[string]interface{}{
				"age": 20,
				"ids": []int{2, 3},
			}},
	}

	for i, c := range cases {
		t.Run(fmt.Sprintf("case%02d", i), func(t *testing.T) {
			t.Logf("before, stmt:%q -- args: %+v", c.stmt, c.args)

			stmt, args, err := sqlx.Named(c.stmt, c.args)
			if err != nil {
				t.Fatalf("Named() unexpected error: %+v", err)
			}
			stmt, args, err = sqlx.In(stmt, args...)
			if err != nil {
				t.Fatalf("In() unexpected error: %+v", err)
			}
			stmt = db.Rebind(stmt)

			var got []User
			t.Logf("after , stmt=%q -- args=%+v", stmt, args)
			if err := db.SelectContext(ctx, &got, stmt, args...); err != nil {
				t.Fatalf("unexpected error: %+v", err)
			}

			if want, got := len(c.want), len(got); want != got {
				t.Errorf("SelectContext() mismatch len(want)=%d != len(got)=%d", want, got)
			}
			type ref struct{ Value []User }
			if diff := cmp.Diff(ref{c.want}, ref{got}, cmpopts.IgnoreFields(User{}, "ID")); diff != "" {
				t.Errorf("SelectContext() mismatch (-want +got):\n%s", diff)
			}
		})
	}
}

func TestInsertMany(t *testing.T) {
	ctx := context.Background()

	db, teardown := newDB(ctx, t, withUsers(nil))
	defer teardown()
	assertCount := func(t *testing.T, prefix string, want int) {
		t.Helper()
		var got int
		stmt := "SELECT COUNT(*) FROM User;"
		if err := db.GetContext(ctx, &got, stmt); err != nil {
			t.Fatalf("%s COUNT(*) unexpected error: %+v", prefix, err)
		}

		if want != got {
			t.Errorf("%s COUNT(*) mismatch want=%d != got=%d", prefix, want, got)
		}
	}

	assertCount(t, "before", 0 /* want */)
	users := []User{
		{Name: "foo", Age: 20},
		{Name: "bar", Age: 20},
	}
	ids := make([]int64, len(users))

	stmt := "INSERT INTO User(name, age) VALUES (?,?),(?,?) RETURNING id as id;"
	if err := db.SelectContext(ctx, &ids, stmt,
		users[0].Name, users[0].Age,
		users[1].Name, users[1].Age,
	); err != nil {
		t.Errorf("SelectContext() unexpected error: %+v", err)
	}
	for i := range users {
		users[i].ID = ids[i] // これ保証ある？
	}
	assertCount(t, "after", 2 /* want */)
}
