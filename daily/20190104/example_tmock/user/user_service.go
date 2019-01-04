package user

//go:generate mocker --dst mock/user_service_mock.go --pkg mock $GOFILE UserService
type UserService interface {
	GetUser(id string) (*User, error)
}
