package main

import (
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"time"
)

type Duration time.Duration

func (d Duration) MarshalJSON() ([]byte, error) {
	return json.Marshal(time.Duration(d).String())
}

func (d *Duration) UnmarshalJSON(b []byte) error {
	var v interface{}
	if err := json.Unmarshal(b, &v); err != nil {
		return err
	}
	switch value := v.(type) {
	case float64:
		*d = Duration(time.Duration(value))
		return nil
	case string:
		tmp, err := time.ParseDuration(value)
		if err != nil {
			return err
		}
		*d = Duration(tmp)
		return nil
	default:
		return errors.New("invalid duration")
	}
}

type S struct {
	Duration Duration `json:"duration"`
}

func main() {
	ob := &S{Duration: Duration(1 * time.Second)}
	fmt.Printf("%#+v\n", ob)

	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", "  ")
	fmt.Println(enc.Encode(ob))

	fmt.Println("--------------")

	var s S
	json.Unmarshal([]byte(`{"duration": "1s"}`), &s)
	fmt.Printf("%#+v\n", s)
}
