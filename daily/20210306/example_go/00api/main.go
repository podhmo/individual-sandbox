package main

import (
	"encoding/json"
	"fmt"
	"io"
	"log"
	"net/http"
	"net/http/httptest"
	"os"
	"reflect"
	"strconv"

	reflectopenapi "github.com/podhmo/reflect-openapi"
	"github.com/podhmo/reflect-openapi/pkg/shape"
)

type User struct {
	Name string `json:"name"`
}

func ListUserWithPagination(page *int) ([]User, Info, error) {
	var i int
	if page != nil {
		i = *page
	}
	switch i {
	case 0:
		return []User{
				{Name: "foo0"},
				{Name: "foo1"},
				{Name: "foo2"},
			},
			Info{HasNext: true, NextID: strconv.Itoa(i + 1)},
			nil
	case 1:
		return []User{
				{Name: "foo3"},
				{Name: "foo4"},
				{Name: "foo5"},
			},
			Info{HasNext: false},
			nil
	default:
		return []User{}, Info{HasNext: false}, nil
	}
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func run() error {
	c := reflectopenapi.Config{}
	c.Selector = &CustomSelector{
		Extractor: c.DefaultExtractor(),
	}
	c.EmitDoc(func(m *reflectopenapi.Manager) {
		op := m.Visitor.VisitFunc(ListUserWithPagination)
		m.Doc.AddOperation("/users", "GET", op)
	})

	fmt.Println("\n----------------------------------------")
	{
		lift := func(sourceFn interface{}) http.HandlerFunc {
			return func(w http.ResponseWriter, r *http.Request) {
				var rargs []reflect.Value
				if page := r.URL.Query().Get("page"); page != "" {
					i, err := strconv.Atoi(page)
					if err != nil {
						// query stringなら無視するか
						log.Printf("400? page=%q", page)
					}
					rargs = append(rargs, reflect.ValueOf(&i))
				}

				xs, info, err := LoadListWithInfo(sourceFn, rargs)
				if err != nil {
					w.WriteHeader(http.StatusInternalServerError)
					fmt.Fprintf(w, `{"message": %q}`, err.Error())
				}
				enc := json.NewEncoder(w)
				// enc.SetIndent("", "  ")
				if err := enc.Encode(struct {
					Items interface{} `json:"items"`
					Info  Info        `json:"info"`
				}{
					Items: xs,
					Info:  info,
				}); err != nil {
					w.WriteHeader(http.StatusInternalServerError)
					fmt.Fprintf(w, `{"message": %q}`, err.Error())
				}
			}
		}

		handler := lift(ListUserWithPagination)

		{
			rec := httptest.NewRecorder()
			req := httptest.NewRequest("GET", "/?page=0", nil)
			handler(rec, req)
			res := rec.Result()
			fmt.Println("API response:", res.Status, req.URL)
			if _, err := io.Copy(os.Stdout, res.Body); err != nil {
				return fmt.Errorf("decode response: %w", err)
			}
			defer res.Body.Close()
		}
		{
			rec := httptest.NewRecorder()
			req := httptest.NewRequest("GET", "/?page=1", nil)
			handler(rec, req)
			res := rec.Result()
			fmt.Println("API response:", res.Status, req.URL)
			if _, err := io.Copy(os.Stdout, res.Body); err != nil {
				return fmt.Errorf("decode response: %w", err)
			}
			defer res.Body.Close()
		}
		{
			rec := httptest.NewRecorder()
			req := httptest.NewRequest("GET", "/?page=2", nil)
			handler(rec, req)
			res := rec.Result()
			fmt.Println("API response:", res.Status, req.URL)
			if _, err := io.Copy(os.Stdout, res.Body); err != nil {
				return fmt.Errorf("decode response: %w", err)
			}
			defer res.Body.Close()
		}
	}
	return nil
}

type CustomSelector struct {
	reflectopenapi.MergeParamsInputSelector // for arglist()
	outputSelector                          reflectopenapi.FirstParamOutputSelector
	Extractor                               reflectopenapi.Extractor
}

// wrap with {"items": <>}
func (s *CustomSelector) SelectOutput(fn shape.Function) shape.Shape {
	out := s.outputSelector.SelectOutput(fn)
	if out, ok := out.(shape.Container); ok && out.GetReflectKind() == reflect.Slice {
		rt := reflect.StructOf([]reflect.StructField{
			{
				Name: "Items",
				Type: out.GetReflectType(),
				Tag:  `json:"items"`,
			},
			{
				Name: "Info",
				Type: reflect.TypeOf(Info{}),
				Tag:  `json:"info"`,
			},
		})
		return s.Extractor.Extract(reflect.New(rt).Interface())
	}
	return out
}

// helpers
type Info struct {
	HasNext bool   `json:"hasNext" required:"true"`
	NextID  string `json:"nextId,omitempty"`
}

// LoadListWithInfo :
// source is func()[]T | func()([]T,error) |  | func()([]T,Info,error)
func LoadListWithInfo(sourceFn interface{}, args []reflect.Value) (interface{}, Info, error) {
	if sourceFn == nil {
		return nil, Info{}, fmt.Errorf("invalid source: %+[1]v is passed, but only supports func()[]<T>", nil)
	}
	rst := reflect.TypeOf(sourceFn)
	if rst.Kind() != reflect.Func {
		return nil, Info{}, fmt.Errorf("invalid source kind: %+[1]v is passed, but only supports func()[]<T>", rst)
	}

	switch rst.NumOut() {
	case 1:
		// func()[]T
		retvals := reflect.ValueOf(sourceFn).Call(args)
		val := retvals[0].Interface()
		return val, Info{}, nil
	case 2:
		// func()([]T, error)
		if !rst.Out(1).Implements(rerrorType) {
			return nil, Info{}, fmt.Errorf("invalid source return type: %+[1]v is passed, but only supports func()([]<T>, error)", rst)
		}
		retvals := reflect.ValueOf(sourceFn).Call(args)
		val := retvals[0].Interface()
		err := retvals[1].Interface()
		if err == nil {
			return val, Info{}, nil
		}
		return val, Info{}, nil
	case 3:
		// func()([]T, Info, error)
		if !rst.Out(2).Implements(rerrorType) {
			return nil, Info{}, fmt.Errorf("invalid source return type: %+[1]v is passed, but only supports func()([]<T>, Info, error)", rst)
		}
		retvals := reflect.ValueOf(sourceFn).Call(args)
		val := retvals[0].Interface()
		info, t := retvals[1].Interface().(Info)
		if !t {
			return nil, Info{}, fmt.Errorf("invalid source info type: %+[1]v is passed, but only supports func()([]<T>, Info, error)", rst)
		}
		err := retvals[2].Interface()
		if err == nil {
			return val, info, nil
		}
		return val, info, err.(error)
	default:
		return nil, Info{}, fmt.Errorf("unexpected source return type: %+[1]v is passed, but only supports func()[]<T>", rst)
	}
}

var rerrorType reflect.Type

func init() {
	rerrorType = reflect.TypeOf(func() error { return nil }).Out(0)
}
