package vanilladi

import (
	"reflect"
	"testing"
)

func TestProduct(t *testing.T) {
	fooBarUser := NewFooBarUser(
		NewFooConst(),
		NewBarNull(),
	)
	fooBarUser.DanceWithDependencies()
}

func Test(t *testing.T) {
	bar := NewBarSpy()
	fooBarUser := NewFooBarUser(
		NewFooStub(1234),
		bar,
	)

	fooBarUser.DanceWithDependencies()
	barSpy, ok := bar.(*barSpy)
	if !ok {
		t.Fatal("expected *barSpy but %T", bar)
	}
	if expected := []int{1234}; !reflect.DeepEqual(barSpy.calledArgs, expected) {
		t.Errorf("expected calledArgs is %v, but got %v", expected, barSpy.calledArgs)
	}
}
