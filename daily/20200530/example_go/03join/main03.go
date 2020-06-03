package main

import (
	"fmt"
	"reflect"
	"strings"

	"github.com/k0kubun/pp"
)

type Table string
type tableLike interface {
	TableName() string
}

func (t Table) Join(rhs tableLike) Table {
	return Table(fmt.Sprintf("%s join %s", t, rhs.TableName()))
}
func (t Table) LeftOuterJoin(rhs tableLike) Table {
	return Table(fmt.Sprintf("%s left outer join %s", t, rhs.TableName()))
}
func (t Table) RightOuterJoin(rhs tableLike) Table {
	return Table(fmt.Sprintf("%s right outer join %s", t, rhs.TableName()))
}
func (t Table) FullOuterJoin(rhs tableLike) Table {
	return Table(fmt.Sprintf("%s full outer join %s", t, rhs.TableName()))
}
func (t Table) TableName() string {
	return string(t)
}

type Field interface {
	Name() string
}
type StringField string

func (f StringField) Name() string {
	return string(f)
}

type People struct {
	Table
	Name StringField
}

func (p *People) As(name string) People {
	new := *p // copy
	Alias(&new, p, name)
	return new
}

func main() {
	people := People{
		Table: "people",
		Name:  StringField("name"),
	}

	p := people.As("p")
	father := people.As("father")

	pp.Println(people, p, father)
	fmt.Println(p.Join(father))
}

func Alias(dst, src interface{}, prefix string) {
	prefix = strings.TrimSuffix(prefix, ".")

	rsrc := reflect.ValueOf(src).Elem()
	rdst := reflect.ValueOf(dst).Elem()

	for i := 0; i < rdst.NumField(); i++ {
		df := rdst.Field(i)

		if df.CanSet() && df.Kind() != reflect.String {
			continue
		}

		ftype := df.Type()
		if ftype.AssignableTo(_rTable) {
			df.SetString(fmt.Sprintf("%s as %s", rsrc.Field(i).String(), prefix))
		} else if ftype.Implements(_rField) {
			df.SetString(prefix + "." + rsrc.Field(i).String())
		}
	}
}

var (
	_rTable = reflect.TypeOf((*Table)(nil)).Elem()
	_rField = reflect.TypeOf((*Field)(nil)).Elem()
)
