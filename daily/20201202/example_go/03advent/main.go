package main

import "fmt"

type Service struct {
	BasePath string
	gw       *gateway

	Team *TeamService
	User *UserService
}

func (s *Service) Do(method string, path string) (string, error) {
	return fmt.Sprintf("%s:	request %s%s", method, s.BasePath, path), nil
}

func New() *Service {
	s := &Service{BasePath: "/api"}
	gw := &gateway{Service: s}
	s.gw = gw

	s.Team = (*TeamService)(gw)
	s.User = (*UserService)(gw)

	return s
}

type gateway struct {
	Service *Service
}

type TeamService gateway

func (s *TeamService) List() (string, error) {
	return s.Service.Do("GET", "/team")
}

type UserService gateway

func (s *UserService) List() (string, error) {
	return s.Service.Do("GET", "/user")
}

func main() {
	s := New()
	fmt.Println(s.Team.List())
	fmt.Println(s.User.List())
}
