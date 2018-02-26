package main

import "time"

// Person :
type Person struct {
	Name      string    `json:"name" minLength:"1" maxLength:"255"`
	NickName  string    `json:"nickname" required:"false" maxLength:"255"`
	Age       int       `json:"age" max:"200" min:"0"`
	CreatedAt time.Time `json:"createdAt"`
	Father    *Person   `json:"father" required:"false"`
	Mother    *Person   `json:"mother" required:"false"`
}
