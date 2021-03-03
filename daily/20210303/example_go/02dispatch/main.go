package main

import (
	"fmt"
	"reflect"
)

type Manager interface {
	CreateTable(name string)
	DropTable(name string)
	RefreshTable(name string)

	Prefix() string

	TypeOf(ob interface{}) string
}

type Base struct {
	manager Manager
}

func (b *Base) CreateTable(name string) {
	// BaseからXのPrefix()が呼べる
	fmt.Printf("%-5s\t%-10s\t%s.%s\n", "in Base", "create table", b.manager.Prefix(), name)
}

func (b *Base) TypeOf(ob interface{}) string {
	return "Unknown"
}

type X struct {
	Base
	prefix string
}

func (x *X) TypeOf(ob interface{}) string {
	// Baseで定義したTypeOfを上書きできる(分岐を増やせる)
	switch reflect.TypeOf(ob).Kind() {
	case reflect.Int:
		return "Integer"
	case reflect.String:
		return "String"
	default:
		return x.Base.TypeOf(ob)
	}
}

func (x *X) RefreshTable(name string) {
	fmt.Println("refresh..")
	x.DropTable(name)   // こちらはXで定義した実装
	x.CreateTable(name) // こちらはBaseで定義したdefault実装
}

func (x *X) DropTable(name string) {
	fmt.Printf("%-5s\t%-10s\t%s.%s\n", "in X", "drop table", x.Prefix(), name)
}
func (x *X) Prefix() string {
	return x.prefix
}

func NewXManager(prefix string) Manager {
	x := &X{prefix: prefix}
	x.Base.manager = x
	return x
}

func main() {
	x := NewXManager("foo")
	x.RefreshTable("Target") // Manager.RefreshTable()

	// xで拡張されたTypeOfが呼べる
	fmt.Println("----------------------------------------")
	fmt.Println("10 is", x.TypeOf(10))
	fmt.Println("foo is", x.TypeOf("foo"))
	fmt.Println("0.1234 is", x.TypeOf(0.1234))
}
