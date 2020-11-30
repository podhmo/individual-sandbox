package main

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"log"
	"reflect"
	"time"
)

type HelloInput struct {
	Name string `json:"name"`
}

func Hello(ctx context.Context, input HelloInput) {
	fmt.Println("----------------------------------------")
	fmt.Printf("Hello %s\n", input.Name)
	fmt.Println("----------------------------------------")
}

type ByeByeInput struct {
	Names []string `json:"names"`
}

func ByeBye(ctx context.Context, input *ByeByeInput) {
	fmt.Println("- - - - - - - - - - - - - - - - - - - - ")
	for _, name := range input.Names {
		fmt.Printf("ByeBye %s\n", name)
	}
	fmt.Println("- - - - - - - - - - - - - - - - - - - - ")
}

func main() {
	ctx := context.Background()
	{
		err := CallX(ctx, Hello, bytes.NewBufferString(`{"name": "foo"}`))
		if err != nil {
			log.Fatalf("!! %+v", err)
		}
	}
	{
		err := CallX(ctx, ByeBye, bytes.NewBufferString(`{"names": ["foo", "bar", "boo"]}`))
		if err != nil {
			log.Fatalf("!! %+v", err)
		}
	}
}

type AnyFunc = interface{}

func CallX(ctx context.Context, fn AnyFunc, r io.Reader) error {
	st := time.Now()
	rfn := reflect.ValueOf(fn)
	log.Printf("start %v", rfn)
	defer func() {
		log.Printf("end %v, elapsed %v", rfn, time.Now().Sub(st))
	}()

	if reflect.Func != rfn.Type().Kind() {
		return fmt.Errorf("invalid kind %s, in %v", rfn.Type().Kind(), rfn)
	}
	// TODO: assert signature
	robType := rfn.Type().In(1)
	usePtr := robType.Kind() == reflect.Ptr
	if usePtr {
		robType = robType.Elem()
	}
	rob := reflect.New(robType)

	ob := rob.Interface()
	decoder := json.NewDecoder(r)
	if err := decoder.Decode(ob); err != nil {
		return err
	}

	if !usePtr {
		rob = rob.Elem()
	}
	// TODO: handling error
	rfn.Call([]reflect.Value{reflect.ValueOf(ctx), rob})
	return nil
}
