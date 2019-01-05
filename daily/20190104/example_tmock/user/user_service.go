package user

//go:generate mocker --dst mock/$GOFILE --pkg mock $GOFILE UserService
type UserService interface {
	GetUser(id string) (*User, error)
}
