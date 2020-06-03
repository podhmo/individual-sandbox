package miniq

import (
	"fmt"
	"reflect"
	"strings"
)

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
