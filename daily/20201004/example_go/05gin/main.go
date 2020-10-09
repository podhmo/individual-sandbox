package main

import (
	"context"
	"encoding/json"
	"log"
	"os"
	"strconv"

	"github.com/gin-gonic/gin"
	reflectopenapi "github.com/podhmo/reflect-openapi"
)

type Server struct {
	Router  gin.IRouter
	Manager *reflectopenapi.Manager
}

func (s *Server) AddEndpoint(method, path string, interactor interface{}, handler func(c *gin.Context)) {
	s.Router.Handle(method, path, handler)
	op := s.Manager.Visitor.VisitFunc(interactor)
	s.Manager.Doc.AddOperation(path, method, op)
}

func main() {
	r := gin.Default()

	c := reflectopenapi.Config{
		//		Resolver: &reflectopenapi.NoRefResolver{},
	}
	doc, err := c.BuildDoc(context.Background(), func(m *reflectopenapi.Manager) {
		s := &Server{Router: r, Manager: m}

		{
			type PingOutput struct {
				Message string `json:"message"`
			}
			s.AddEndpoint(
				"GET", "/ping",
				func() *PingOutput { return nil },
				func(c *gin.Context) {
					c.JSON(200, gin.H{
						"message": "pong",
					})
				},
			)
		}
	})
	if err != nil {
		log.Fatalf("doc %+v", err)
	}

	addr := os.Getenv("ADDR")
	if addr == "" {
		addr = ":8080"
	}

	if ok, _ := strconv.ParseBool(os.Getenv("DOCGEN")); ok {
		enc := json.NewEncoder(os.Stdout)
		enc.SetIndent("", "  ")
		if err := enc.Encode(doc); err != nil {
			log.Fatalf("encode %+v", err)
		}
		return
	}
	r.Run(addr) // listen and serve on 0.0.0.0:8080 (for windows "localhost:8080")
}
