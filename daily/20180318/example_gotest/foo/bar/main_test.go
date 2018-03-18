package bar

import (
	"fmt"
	"reflect"
	"testing"
)

func TestMain(m *testing.M) {
	m.Run()
	fmt.Printf("end tests=%#+v\n", reflect.ValueOf(m).Elem().FieldByName("tests"))
}
