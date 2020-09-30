package main

import (
	"context"
	"fmt"
	"log"
	"net/http"
	"os"
	"strings"

	"github.com/go-chi/chi"
	"github.com/go-chi/chi/middleware"
	"github.com/podhmo/tenuki"
)

type UserID string
type APIKey struct {
	Token     string
	CreatedBy UserID
}

type KeyStore struct {
	m map[string]APIKey
}

func (s *KeyStore) GenerateKey(userID UserID) APIKey {
	k := APIKey{
		Token:     fmt.Sprintf("ak:%s", userID),
		CreatedBy: userID,
	} // xxx
	s.m[k.Token] = k
	return k
}

type Role string

const (
	RoleAnonymouns Role = ""
	RoleUser       Role = "user"
	RoleAdmin      Role = "admin"
)

type UserRole struct {
	APIKey APIKey
	Role   Role
}

func (ur UserRole) IsAnonymous() bool {
	return ur.Role == RoleAnonymouns
}

func (s *KeyStore) GetUserRole(token string, userID UserID) UserRole {
	key, ok := s.m[token]
	if !ok {
		return UserRole{Role: RoleAnonymouns}
	}

	role := RoleUser
	if key.CreatedBy == userID {
		role = RoleAdmin
	}
	return UserRole{
		Role:   role,
		APIKey: key,
	}
}

type Server struct {
	http.Handler
	KeyStore *KeyStore
}

type contextKey string

const (
	contextKeyUserRole contextKey = "userRole"
)

func (s *Server) GetUserID(r *http.Request) UserID {
	return UserID("xxx") // TODO:
}
func (s *Server) GetUserRole(r *http.Request) UserRole {
	v := r.Context().Value(contextKeyUserRole)
	if v != nil {
		return v.(UserRole)
	}

	text := r.Header.Get("Authorization")
	if text == "" {
		return s.KeyStore.GetUserRole("", UserID(""))
	}

	userID := s.GetUserID(r)
	key := strings.Split(text, " ")[1] // Bearer <xxx>
	role := s.KeyStore.GetUserRole(key, userID)
	r.WithContext(context.WithValue(r.Context(), contextKeyUserRole, role))
	return role
}

func (s *Server) Hello(w http.ResponseWriter, r *http.Request) {
	role := s.GetUserRole(r)
	if role.IsAnonymous() {
		tenuki.Render(w, r).JSON(401, map[string]string{"message": http.StatusText(401)})
		return
	}
	tenuki.Render(w, r).JSON(200, map[string]interface{}{
		"role":    role.Role,
		"message": "hello",
	})
}

func NewKeyStore() *KeyStore {
	store := &KeyStore{m: map[string]APIKey{}}

	// for debug
	fmt.Fprintln(os.Stderr, "** generate key", store.GenerateKey("user:0"))

	return store
}
func NewRouter() chi.Router {
	r := chi.NewRouter()
	r.Use(middleware.Logger)
	return r
}
func NewServer(r chi.Router) http.Handler {
	s := &Server{
		Handler:  r,
		KeyStore: NewKeyStore(),
	}

	r.Get("/hello", s.Hello)

	return s
}

func main() {
	srv := NewServer(NewRouter())
	addr := os.Getenv("ADDR")
	if addr == "" {
		addr = ":4444"
	}

	log.Printf("listen: %s", addr)
	if err := http.ListenAndServe(addr, srv); err != nil {
		log.Fatalf("!! %+v", err)
	}
}
