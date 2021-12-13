package main

import (
	"context"
	"fmt"
	"net/http"
)

type Person struct {
	ID       string
	Name     string
	FatherID string
	MotherID string
}

type DB struct {
	People map[string]*Person
}

var ErrNotFound = fmt.Errorf("404")

// resolver
type PersonView struct {
	Person
	Father   *PersonView   `json:"father,omitempty"`
	Mother   *PersonView   `json:"mother,omitempty"`
	Children []*PersonView `json:"children,omitempty"`
}

type PersonResolver struct {
	DB *DB
}

func (r *PersonResolver) Person(id string) (*Person, error) {
	db := r.DB
	ob, ok := db.People[id]
	if !ok {
		return nil, ErrNotFound
	}
	return ob, nil
}
func (r *PersonResolver) Father(ob *Person) (*Person, error) {
	return r.Person(ob.FatherID)
}
func (r *PersonResolver) Mother(ob *Person) (*Person, error) {
	return r.Person(ob.FatherID)
}
func (r *PersonResolver) Children(ob *Person) ([]*Person, error) {
	var children []*Person
	for _, x := range r.DB.People {
		if x.FatherID == ob.ID || x.MotherID == ob.ID {
			children = append(children, x)
		}
	}
	return children, nil
}

type PersonSerializer struct {
	*PersonResolver
}

func (s *PersonSerializer) Person(x *Person) *PersonView {
	return &PersonView{Person: *x}
}
func (s *PersonSerializer) People(xs []*Person) []*PersonView {
	r := make([]*PersonView, len(xs))
	for i, x := range xs {
		r[i] = s.Person(x)
	}
	return r
}
func (s *PersonSerializer) FullSet(ob *Person, father *Person, mother *Person, children []*Person) *PersonView {
	v := s.Person(ob)
	v.Father = s.Person(father)
	v.Mother = s.Person(mother)
	v.Children = s.People(children)
	return v
}

// web

type APIRouter interface {
	Get(path string, handler http.HandlerFunc)
}

func GetPathParam(r *http.Request, k string) string {
	panic("not implemented")
	return "" // TODO: implement
}
func HandleResult(w http.ResponseWriter, req *http.Request, ob interface{}, err error, status int) {
	panic("not implemented")
}

func Mount(
	r APIRouter,
	PersonResolver *PersonResolver,
	PersonSerializer *PersonSerializer,
) {
	r.Get("/person/{id}", func(w http.ResponseWriter, req *http.Request) {
		id := GetPathParam(req, "id")
		ob, err := PersonResolver.Person(id)
		if err != nil {
			HandleResult(w, req, nil, err, 400)
			return
		}
		r := PersonSerializer.Person(ob)
		HandleResult(w, req, r, nil, 200)
	})

	r.Get("/person/{id}/father", func(w http.ResponseWriter, req *http.Request) {
		id := GetPathParam(req, "id")
		ob, err := PersonResolver.Person(id)
		if err != nil {
			HandleResult(w, req, nil, err, 404)
			return
		}
		father, err := PersonResolver.Father(ob)
		if err != nil {
			HandleResult(w, req, nil, err, 404)
			return
		}
		r := PersonSerializer.Person(father)
		HandleResult(w, req, r, nil, 200)
	})

	r.Get("/person/{id}/mother", func(w http.ResponseWriter, req *http.Request) {
		id := GetPathParam(req, "id")
		ob, err := PersonResolver.Person(id)
		if err != nil {
			HandleResult(w, req, nil, err, 404)
			return
		}
		mother, err := PersonResolver.Mother(ob)
		if err != nil {
			HandleResult(w, req, nil, err, 404)
			return
		}
		r := PersonSerializer.Person(mother)
		HandleResult(w, req, r, nil, 200)
	})

	r.Get("/person/{id}/fullset", func(w http.ResponseWriter, req *http.Request) {
		id := GetPathParam(req, "id")
		ob, err := PersonResolver.Person(id)
		if err != nil {
			HandleResult(w, req, nil, err, 404)
			return
		}
		// need optmize?

		father, err := PersonResolver.Father(ob)
		if err != nil {
			HandleResult(w, req, nil, err, 404)
			return
		}
		mother, err := PersonResolver.Mother(ob)
		if err != nil {
			HandleResult(w, req, nil, err, 404)
			return
		}
		children, err := PersonResolver.Children(ob)
		if err != nil {
			HandleResult(w, req, nil, err, 404)
			return
		}
		r := PersonSerializer.FullSet(ob, father, mother, children)
		HandleResult(w, req, r, nil, 200)
	})
}

// graphql
type GraphManager interface {
	Query(options ...Option)
	Object(ob interface{}, options ...Option)
	Field(name string, resolve ResolveFunc) Option
	Ignore(names ...string) Option

	// TODO: mutation
	// TODO: union, oneOf, implements
}
type ResolveFunc interface{}
type Option interface{}

func GetLoginID(ctx context.Context) string {
	panic("not impelemented")
	return ""
}

func GetArg(ctx context.Context, k string) interface{} {
	panic("not impelmented")
}
func GetCurrent(ctx context.Context) interface{} {
	panic("not impelmented")
}
func Graph(
	m GraphManager,
	PersonResolver *PersonResolver,
) {
	m.Query(
		m.Field("view", func(ctx context.Context) (*Person, error) {
			myID := GetLoginID(ctx)
			return PersonResolver.Person(myID)
		}),
		m.Field("person", func(ctx context.Context) (*Person, error) {
			id := GetArg(ctx, "id").(string)
			return PersonResolver.Person(id)
		}),
	)

	m.Object(&Person{}, m.Ignore("FatherID", "MotherID"),
		m.Field("father", func(ctx context.Context) (interface{}, error) {
			ob := GetCurrent(ctx).(*Person)
			return PersonResolver.Father(ob)
		}),
		m.Field("mother", func(ctx context.Context) (interface{}, error) {
			ob := GetCurrent(ctx).(*Person)
			return PersonResolver.Mother(ob)
		}),
		m.Field("children", func(ctx context.Context) (interface{}, error) {
			ob := GetCurrent(ctx).(*Person)
			return PersonResolver.Children(ob)
		}),
	)
}

func main() {}
