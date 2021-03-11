package main

import (
	"context"
	"fmt"
	"log"
	"runtime/debug"
	"unsafe"
)

func RecovererCtx(ctx context.Context, handler func(r interface{}, trace string)) func() {
	return func() {
		r := recover()
		if r == nil {
			return
		}

		trace := debug.Stack()

		if handler != nil {
			handler(r, (*(*string)(unsafe.Pointer(&trace))))
		}
		log.Printf("stackSize=%d stack=%s", len(trace), (*(*string)(unsafe.Pointer(&trace))))
	}
}
func main() {
	ctx := context.Background()
	err := func() (retErr error) {
		recoverer := RecovererCtx(ctx, func(r interface{}, trace string) {
			switch tr := r.(type) {
			case error:
				retErr = tr
				return
			default:
				retErr = fmt.Errorf("%+v", r)
				return
			}
		})
		defer recoverer()
		return nil
	}()
	fmt.Println(err)
}
