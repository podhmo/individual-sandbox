package main

import (
	"fmt"
	"log"
	"reflect"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	{
		var result []int
		fmt.Printf("type:%T	value:%v	panic?:%v\n", result, result, isUnexpected(result))
		fmt.Printf("type:%T	value:%v	panic?:%v\n", &result, &result, isUnexpected(&result))
	}
	{
		result := map[string]string{}
		fmt.Printf("type:%T	value:%v	panic?:%v\n", result, result, isUnexpected(result))
		fmt.Printf("type:%T	value:%v	panic?:%v\n", &result, &result, isUnexpected(&result))
	}
	{
		var result map[string]string
		fmt.Printf("type:%T	value:%v	panic?:%v\n", result, result, isUnexpected(result))
		fmt.Printf("type:%T	value:%v	panic?:%v\n", &result, &result, isUnexpected(&result))
	}
	return nil
}

func isUnexpected(result interface{}) bool {
	resultv := reflect.ValueOf(result)
	return resultv.Kind() != reflect.Ptr || resultv.Elem().Kind() != reflect.Slice
}
