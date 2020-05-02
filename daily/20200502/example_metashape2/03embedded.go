package main

type Person struct {
	Name string
	Age int
	Memo  // {'inline': True, 'required': True}
}

type Memo struct {
	Data map[string]interface{}
}
