package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"log"
	"os"
)

// Person ...
type Person struct {
	Name string `json:"name"`
	Age  int    `json:"age"`

	// data additional properties
	AdditionalProperties map[string]interface{} `json:"-"`
}

// UnmarshalJSON unmarshals this object with additional properties from JSON
func (m *Person) UnmarshalJSON(data []byte) error {
	// stage 1, bind the properties
	var stage1 struct {
		Name string `json:"name"`
		Age  int    `json:"age"`
	}
	if err := json.Unmarshal(data, &stage1); err != nil {
		return err
	}
	var rcv Person

	rcv.Name = stage1.Name
	rcv.Age = stage1.Age

	*m = rcv

	// stage 2, remove properties and add to map
	stage2 := make(map[string]json.RawMessage)
	if err := json.Unmarshal(data, &stage2); err != nil {
		return err
	}

	delete(stage2, "name")
	delete(stage2, "age")

	// stage 3, add additional properties values
	if len(stage2) > 0 {
		result := make(map[string]interface{})
		for k, v := range stage2 {
			var toadd interface{}
			if err := json.Unmarshal(v, &toadd); err != nil {
				return err
			}
			result[k] = toadd
		}
		m.AdditionalProperties = result
	}

	return nil
}

// MarshalJSON marshals this object with additional properties into a JSON object
func (m Person) MarshalJSON() ([]byte, error) {
	var stage1 struct {
		Name string `json:"name"`
		Age  int    `json:"age"`
	}

	stage1.Name = m.Name
	stage1.Age = m.Age

	// make JSON object for known properties
	props, err := json.Marshal(stage1)
	if err != nil {
		return nil, err
	}

	if len(m.AdditionalProperties) == 0 {
		return props, nil
	}

	// make JSON object for the additional properties
	additional, err := json.Marshal(m.AdditionalProperties)
	if err != nil {
		return nil, err
	}

	if len(props) < 3 {
		return additional, nil
	}

	// concatenate the 2 objects
	props[len(props)-1] = ','
	return append(props, additional[1:]...), nil
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("!%+v", err)
	}
}

func run() error {
	code := `
{
  "name": "foo",
  "age": 20,
  "nickname": "F"
}
`
	fmt.Println("input:")
	fmt.Println(code)
	fmt.Println("")

	o := bytes.NewBufferString(code)
	decoder := json.NewDecoder(o)

	var ob Person
	if err := decoder.Decode(&ob); err != nil {
		return err
	}

	fmt.Println("got:")
	fmt.Printf("%#+v\n", ob)

	fmt.Println("")
	fmt.Println("output:")

	encoder := json.NewEncoder(os.Stdout)
	if err := encoder.Encode(&ob); err != nil {
		return err
	}
	return nil
}
