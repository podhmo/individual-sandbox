package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"reflect"
)

type S struct{}

func ReturnNil() ([]*S, error) {
	return nil, nil
}

func Use() {
	result, err := ReturnNil()
	HandleResult(result, err)
}

func toJSON(ob interface{}) string {
	var buf bytes.Buffer
	if err := json.NewEncoder(&buf).Encode(ob); err != nil {
		panic(err)
	}
	return buf.String()
}

func HandleResult(result interface{}, err error) {
	target := result
	fmt.Printf("%[1]T, %+#[1]v, isNil=%[2]v, json=%[3]s\n", target, target == nil, toJSON(target))

	val := reflect.ValueOf(result)
	if val.Kind() == reflect.Slice && val.IsNil() {
		target = reflect.MakeSlice(val.Type(), 0, 0).Interface()
	}
	fmt.Printf("%[1]T, %+#[1]v, isNil=%[2]v, json=%[3]s\n", target, target == nil, toJSON(target))
}

func main() {
	Use()
}

// []*main.S, []*main.S(nil), isNil=false, json=null
// []*main.S, []*main.S{}, isNil=false, json=[]

