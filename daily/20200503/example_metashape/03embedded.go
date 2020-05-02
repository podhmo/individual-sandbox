package main

type Person struct {
	Name string
	Age int
	Memo  // {'inline': True, 'required': True, 'comment': ''}
}

type Memo struct {
	Data map[string]interface{}
}
