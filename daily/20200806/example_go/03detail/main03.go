package main

import (
	"fmt"
	"reflect"
	"unsafe"
)

type S struct {
	Exported   int
	unexported int
}

type flag uintptr

const (
	flagKindWidth        = 5 // there are 27 kinds
	flagKindMask    flag = 1<<flagKindWidth - 1
	flagStickyRO    flag = 1 << 5 // a
	flagEmbedRO     flag = 1 << 6 // b
	flagIndir       flag = 1 << 7 // c
	flagAddr        flag = 1 << 8 // d
	flagMethod      flag = 1 << 9 // e
	flagMethodShift      = 10
	flagRO          flag = flagStickyRO | flagEmbedRO
)

func checkFlag(flag flag) {
	fmt.Println("flagStickyRO:", flag&flagStickyRO != 0)
	fmt.Println("flagEmbedRO:", flag&flagEmbedRO != 0)
	fmt.Println("flagIndir:", flag&flagIndir != 0)
	fmt.Println("flagAddr:", flag&flagAddr != 0)
	fmt.Println("flagMethod:", flag&flagMethod != 0)
	fmt.Println("flagMethodShift:", flag&flagMethodShift != 0)
	fmt.Println("flagRO:", flag&flagRO != 0)
}

func main() {
	s := &S{Exported: 20, unexported: 10}
	rv := reflect.ValueOf(s).Elem()

	rf := rv.FieldByName("unexported")
	checkFlag(flag(reflect.ValueOf(rf).FieldByName("flag").Uint()))
	fmt.Println("----------------------------------------")
	rf = reflect.NewAt(rf.Type(), unsafe.Pointer(rf.UnsafeAddr())).Elem()
	checkFlag(flag(reflect.ValueOf(rf).FieldByName("flag").Uint()))
	fmt.Println("----------------------------------------")
	checkFlag(flag(reflect.Ptr))
	fmt.Println(rf.Interface())
}
