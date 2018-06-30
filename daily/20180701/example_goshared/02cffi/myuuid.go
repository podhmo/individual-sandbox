package main

// #include <stdlib.h>
import "C"
import (
	"unsafe"

	"github.com/satori/go.uuid"
)

var pool map[uintptr]*C.char

func init() {
	pool = map[uintptr]*C.char{}
}

// Gen :
//export Gen
func Gen() (*C.char, uintptr) {
	u := uuid.Must(uuid.NewV4()).String()
	v := C.CString(u)
	k := uintptr(unsafe.Pointer(v))
	pool[k] = v
	return v, k
}

// Free :
//export Free
func Free(k uintptr) {
	v := pool[k]
	delete(pool, k)
	C.free(unsafe.Pointer(v))
}

func main() {
}
