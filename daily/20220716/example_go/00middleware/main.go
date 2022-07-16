package main

import (
	"context"
	_ "embed"
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"net/http/httptest"
	"reflect"
	"strings"

	"github.com/getkin/kin-openapi/openapi3"
	"github.com/getkin/kin-openapi/openapi3filter"
	"github.com/getkin/kin-openapi/routers"
	"github.com/getkin/kin-openapi/routers/gorillamux"
)

//go:embed openapi.json
var spec []byte

func main() {
	log.SetFlags(0)
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func run() error {
	ctx := context.Background()
	baseURL := "http://localhost:8080"

	doc, err := openapi3.NewLoader().LoadFromData(spec)
	if err != nil {
		return fmt.Errorf("load doc: %w", err)
	}
	if doc.Validate(ctx); err != nil {
		return fmt.Errorf("validate doc: %w", err)
	}

	router, err := gorillamux.NewRouter(doc)
	if err != nil {
		return fmt.Errorf("new router: %w", err)
	}

	{
		log.Println("ng request ----------------------------------------")
		req, err := http.NewRequest("POST", baseURL+"/pets", strings.NewReader(`{}`))
		if err != nil {
			return fmt.Errorf("new request: %w", err)
		}
		req.Header.Set("Content-Type", "application/json")
		if err := doRequest(ctx, router, req); err != nil {
			log.Println(err)
		}
	}
	{
		log.Println("ng response ----------------------------------------")
		req, err := http.NewRequest("POST", baseURL+"/pets", strings.NewReader(`{"name": "foo"}`))
		if err != nil {
			return fmt.Errorf("new request: %w", err)
		}
		req.Header.Set("Content-Type", "application/json")
		if err := doRequest(ctx, router, req); err != nil {
			log.Println(err)
		}
	}
	return nil
}

func doRequest(ctx context.Context, router routers.Router, req *http.Request) error {
	route, pathParams, err := router.FindRoute(req)
	if err != nil {
		return fmt.Errorf("find route: %w", err)
	}
	log.Println("find route:", route.Path, route.Method, pathParams)

	reqInput := &openapi3filter.RequestValidationInput{
		Request:     req,
		PathParams:  pathParams,
		QueryParams: req.URL.Query(),
		Route:       route,
		// Options: nil, // ?
		// ParamDecoder: nil, // ?
	}
	if err := openapi3filter.ValidateRequest(ctx, reqInput); err != nil {
		log.Printf("validate request is failed: %T", err)
		return fmt.Errorf("validate request: %w", err)
	}
	log.Println("request is ok")

	rec := httptest.NewRecorder()
	func(w http.ResponseWriter, req *http.Request) {
		w.Header().Add("Content-Type", "application/json")
		json.NewEncoder(w).Encode(map[string]interface{}{})
	}(rec, req)

	res := rec.Result()
	resInput := &openapi3filter.ResponseValidationInput{
		RequestValidationInput: reqInput,
		Status:                 200,
		Header:                 res.Header,
		Body:                   res.Body,
		Options:                nil, // ?
	}
	if err := openapi3filter.ValidateResponse(ctx, resInput); err != nil {
		log.Printf("valicate response is failed: %T", err)
		return fmt.Errorf("validate response: %w", err)
	}
	return nil
}

func dumpRoutes(doc *openapi3.T) {
	expectType := reflect.TypeOf(&openapi3.Operation{})
	for k, path := range doc.Paths {
		rv := reflect.ValueOf(path).Elem()
		rt := reflect.TypeOf(path).Elem()
		for i := 0; i < rt.NumField(); i++ {
			rf := rt.Field(i)
			if !rf.Type.AssignableTo(expectType) {
				continue
			}
			rfv := rv.Field(i)
			if rfv.IsNil() {
				continue
			}
			op := rfv.Interface().(*openapi3.Operation)
			fmt.Printf("%-10s\t%-10s\t%s\n", k, rf.Name, op.OperationID)
		}
	}
}
