package db

type User struct {
	ID   int    `json:"id"`
	Name string `json:"Name"`
}

var users []User

func Clear() {
	users = nil
}
func GetUsers() []User {
	return users
}
func InsertUser(user User) error {
	users = append(users, user)
	return nil
}
