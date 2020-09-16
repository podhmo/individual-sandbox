package main

import (
	"context"
	"errors"
	"log"
	"net"
	"os"

	"m/store"
	"m/todorpc"

	"github.com/golang/protobuf/ptypes/empty"
	"google.golang.org/grpc"
)

// server is used to implement 	todorpc.TodoBoardServer
type server struct {
	store *store.TodoStore
}

func (s *server) Add(ctx context.Context, in *todorpc.Todo) (*empty.Empty, error) {
	if in == nil {
		return nil, errors.New("nil")
	}
	log.Printf("Received:%#+v", in)
	s.store.Add(store.Todo{
		Title: in.GetTitle(),
		Done:  in.GetDone(),
	})
	return &empty.Empty{}, nil
}
func (s *server) List(ctx context.Context, in *empty.Empty) (*todorpc.TodoList, error) {
	items := s.store.List()
	r := make([]*todorpc.Todo, len(items))
	for i := range items {
		r[i] = &todorpc.Todo{
			Title: items[i].Title,
			Done:  items[i].Done,
		}
	}
	return &todorpc.TodoList{Todos: r}, nil
}

func main() {
	port := os.Getenv("PORT")
	if port == "" {
		port = ":50051"
	}

	log.Println("listening ...", port)
	lis, err := net.Listen("tcp", port)
	if err != nil {
		log.Fatalf("failed to listen: %v", err)
	}
	s := grpc.NewServer()
	todorpc.RegisterTodoBoardServer(s, &server{
		store: store.NewTodoStore(),
	})
	if err := s.Serve(lis); err != nil {
		log.Fatalf("failed to serve: %v", err)
	}
}
