package main

import (
	"fmt"
	"reflect"
	"runtime"
	"unsafe"
	_ "unsafe"
)

type flag uintptr

const (
	flagKindWidth        = 5 // there are 27 kinds
	flagKindMask    flag = 1<<flagKindWidth - 1
	flagStickyRO    flag = 1 << 5
	flagEmbedRO     flag = 1 << 6
	flagIndir       flag = 1 << 7
	flagAddr        flag = 1 << 8
	flagMethod      flag = 1 << 9
	flagMethodShift      = 10
	flagRO          flag = flagStickyRO | flagEmbedRO
)

type P struct{}

func (p *P) Foo() {
	fmt.Println("@@")
}

//go:linkname reflect_pointer reflect.Value.pointer
func reflect_pointer(v reflect.Value) unsafe.Pointer

func main() {
	p := new(P)
	{
		rv := reflect.ValueOf(p.Foo)
		fmt.Println(rv, p.Foo, fmt.Sprintf("%[1]T, %+#[1]v", p.Foo))
		//		pp.Println(rv)
		ptr := rv.Pointer()
		fmt.Println(ptr)

		rfunc := runtime.FuncForPC(ptr)
		fname, lineno := rfunc.FileLine(rfunc.Entry())
		fmt.Println(rfunc.Name(), fname, lineno)

	}
	fmt.Println("----------------------------------------")
	{
		rv := reflect.ValueOf(p)
		foo := rv.MethodByName("Foo")
		rv = foo
		fmt.Println(rv, rv.Interface(), fmt.Sprintf("%[1]T, %+#[1]v", rv.Interface()))
		//		pp.Println(rv)
		ptr := rv.Pointer()
		fmt.Println(ptr)

		rfunc := runtime.FuncForPC(ptr)
		fname, lineno := rfunc.FileLine(rfunc.Entry())
		fmt.Println(rfunc.Name(), fname, lineno)

	}
	fmt.Println("----------------------------------------")
	{
		rv := reflect.ValueOf(p)
		{
			foo := rv.MethodByName("Foo")
			fmt.Println(foo)
			foo.Call(nil)
			fmt.Println("@@@", reflect_pointer(rv), reflect_pointer(foo))
			// methodの元のstructのものなのか
			// どのメソッドを呼ぶかはどこで管理されているかというと、flagの部分

			// return v.call("Call", in)
			// rcvrtype, t, fn = methodReceiver(op, v, int(v.flag)>>flagMethodShift)

			// in mehodReceiver
			//
			// } else {
			// 	rcvrtype = v.typ
			// 	ms := v.typ.exportedMethods()
			// 	if uint(i) >= uint(len(ms)) {
			// 		panic("reflect: internal error: invalid method index")
			// 	}
			// 	m := ms[i]
			// 	if !v.typ.nameOff(m.name).isExported() {
			// 		panic("reflect: " + op + " of unexported method")
			// 	}
			// 	ifn := v.typ.textOff(m.ifn)
			// 	fn = unsafe.Pointer(&ifn)
			// 	t = (*funcType)(unsafe.Pointer(v.typ.typeOff(m.mtyp)))
			// }
				
			// textOff, exportedMethods() だけで十分では？
		}
		{
			foo := reflect.ValueOf(p.Foo)
			fmt.Println(foo)
		}

		foo := reflect.ValueOf(p.Foo)
		rv = foo
		p := reflect_pointer(rv)
		p = *(*unsafe.Pointer)(p)
		ptr := uintptr(p)
		// ptr := rv.Pointer()
		fmt.Println(ptr)

		rfunc := runtime.FuncForPC(ptr)
		fname, lineno := rfunc.FileLine(rfunc.Entry())
		fmt.Println(rfunc.Name(), fname, lineno)

	}
}
