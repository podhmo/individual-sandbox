package main

import (
	"context"
	"fmt"
	"sync/atomic"
	"testing"
)

func TestIt(t *testing.T) {
	s := getDBSession(t)
	ctx := context.Background()

	if _, err := s.Users.Insert(
		ctx, []User{
			{Name: "foo"},
			{Name: "bar"},
			{Name: "boo"},
		}...); err != nil {
		t.Error(err)
	}

	var c int64
	if err := s.DB.GetContext(ctx, &c, "select count(*) from user where tenant_id=?", s.Tenant.ID); err != nil {
		t.Errorf("db check, unexpected error %+v", err)
	}
	fmt.Println("@", c)
}
func TestIt2(t *testing.T) {
	s := getDBSession(t)
	ctx := context.Background()

	if _, err := s.Users.Insert(
		ctx, []User{
			{Name: "foo"},
			{Name: "bar"},
		}...); err != nil {
		t.Error(err)
	}

	var c int64
	if err := s.DB.GetContext(ctx, &c, "select count(*) from user where tenant_id=?", s.Tenant.ID); err != nil {
		t.Errorf("db check, unexpected error %+v", err)
	}
	fmt.Println("@", c)
}

func TestMain(m *testing.M) {
	defer setupDBManager()()
	m.Run()
}

var (
	getDBSession func(t *testing.T) *DBSession
	i            int64 = 0
)

func setupDBManager() func() {
	ctx := context.Background()
	m, err := NewDBManager(ctx, "sqlite3", ":memory:")
	if err != nil {
		panic(err)
	}
	if err := setupDB(ctx, m); err != nil {
		panic(err)
	}

	getDBSession = func(t *testing.T) *DBSession {
		// 各テスト関数の中では、dbのsetupもteardownも行わない。その代わり全てのレコードはこのSession経由で作成される。tenantIdで隔離されている。
		i := atomic.AddInt64(&i, 1)
		s, err := NewDBSession(ctx, m, fmt.Sprintf("tenant:%d", i))
		if err != nil {
			t.Fatalf("new db session is failed, something wrong -- %+v", err)
		}
		return s
	}
	return func() {}
}
