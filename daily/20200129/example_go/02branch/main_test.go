package main

import (
	"fmt"
	"m/02branch/branch"
	"reflect"
	"testing"
)

func Test(t *testing.T) {
	xtype := branch.XTypeBoo
	cmd := branch.New()

	called := false
	cmd.Cont = func(xtype branch.XType) error {
		called = true
		return nil
	}

	err := cmd.Do(xtype)
	if err != nil {
		t.Fatalf("never %s", err)
	}
	if !called {
		t.Error("cont is not called")
	}
}

func TestWithReflect(t *testing.T) {
	// but cannot access unexported field, so useless
	xtype := branch.XTypeBoo
	called := false

	cmd := branch.New()

	rv := reflect.ValueOf(cmd).Elem()
	fmt.Println(rv.NumField())
	f := rv.FieldByName("Cont")
	var icont interface{} = func(xtype branch.XType) error {
		called = true
		return nil
	}
	rcont := reflect.ValueOf(icont)
	if rcont.Kind() != reflect.Func {
		panic("must func")
	}
	f.Set(rcont)

	err := cmd.Do(xtype)
	if err != nil {
		t.Fatalf("never %s", err)
	}
	if !called {
		t.Error("cont is not called")
	}
}
