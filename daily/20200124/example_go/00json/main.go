package main

import (
	"encoding/json"
	"fmt"
)

type Value struct {
	Name  string  `json:"name"`
	Value float64 `json:"value"`
}

type RValue struct {
	Name  string `json:"name"`
	Value int64  `json:"value"`
}

func main() {
	{
		v := Value{
			Name:  "foo",
			Value: 2.123,
		}
		b, _ := json.Marshal(v)
		fmt.Println(string(b))
		var rv RValue
		json.Unmarshal(b, &rv)
		fmt.Println(rv)
	}

	{
		s := `{"name": "bar", "value": 199.04527018639698}`
		var rv RValue
		json.Unmarshal([]byte(s), &rv)
		fmt.Println(rv)
	}

	{
		v := Value{
			Name:  "foo",
			Value: 2.123,
		}
		wv := struct {
			*Value
			WValue int64 `json:"value"`
		}{
			Value:  &v,
			WValue: int64(v.Value),
		}

		b, _ := json.Marshal(wv)
		fmt.Println(string(b))
		var rv RValue
		json.Unmarshal(b, &rv)
		fmt.Println(rv)
	}
}
