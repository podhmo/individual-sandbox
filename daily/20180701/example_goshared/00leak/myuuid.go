package main

import "C"
import (
	"github.com/satori/go.uuid"
)

// Gen :
//export Gen
func Gen() *C.char {
	u := uuid.Must(uuid.NewV4()).String()
	// memory leak?
	v := C.CString(u)
	return v
}

func main() {
}
