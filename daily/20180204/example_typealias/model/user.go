package model

// User :
type User struct {
	Name string `json:"name"`
	Age  int    `json:"age"`
}

func (u *User) Hello() string {
	return "hello from model"
}
