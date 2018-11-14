package main

import (
	"fmt"
	"net/http/httptest"
	"reflect"

	"github.com/sirupsen/logrus"
)

func main() {
	fmt.Println(reflect.Indirect(reflect.ValueOf(logrus.Logger{})).Type())
	fmt.Println(reflect.Indirect(reflect.ValueOf(logrus.Logger{})).Type().PkgPath())
	fmt.Println(reflect.Indirect(reflect.ValueOf(httptest.NewRecorder())).Type())
}
