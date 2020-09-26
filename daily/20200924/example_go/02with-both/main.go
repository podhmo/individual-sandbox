// +build !lambda

package main

import (
	"encoding/json"
	"log"
	"net/http"
	"os"

	"github.com/go-chi/chi"
	"github.com/go-chi/render"
)

type Controller struct {
	Interactor *Interactor
}

func (c *Controller) List(w http.ResponseWriter, r *http.Request) {
	items, err := c.Interactor.List()
	if err != nil {
		render.Status(r, 500)
		render.JSON(w, r, map[string]interface{}{"message": err.Error()})
		return
	}
	render.JSON(w, r, map[string]interface{}{"items": items})
}

func (c *Controller) Add(w http.ResponseWriter, r *http.Request) {
	var item Todo
	decoder := json.NewDecoder(r.Body)
	if err := decoder.Decode(&item); err != nil {
		render.Status(r, 400)
		render.JSON(w, r, map[string]interface{}{"message": err.Error()})
	}

	added, err := c.Interactor.Add(item)
	if err != nil {
		render.Status(r, 500)
		render.JSON(w, r, map[string]interface{}{"message": err.Error()})
		return
	}
	render.JSON(w, r, added)
}

func main() {
	r := chi.NewRouter()

	// このままが良いのか、interactorの粒度を細かくしたほうが良いのかはわからない
	// github.com/awslabs/aws-lambda-go-api-proxy を使うよりは意義があるのではないか？
	// Controllerが肥大化するならたぶん分けたほうが良い。Interactorも同様ならhandler関数を生成する関数を定義する形のほうが綺麗かもしれない

	c := &Controller{Interactor: GetInteractor()}
	r.Get("/", c.List)
	r.Post("/", c.Add)

	addr := os.Getenv("ADDR")
	if addr == "" {
		addr = ":44444"
	}

	if err := http.ListenAndServe(addr, r); err != nil {
		log.Fatalf("!!%+v", err)
	}
}
