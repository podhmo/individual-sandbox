package main

import (
	"context"
	"fmt"
	"log"
	"os"
	"strings"
	"time"

	"m/todorpc"

	"github.com/golang/protobuf/ptypes/empty"
	"google.golang.org/grpc"
)

func main() {
	host := os.Getenv("HOST")
	if host == "" {
		host = "localhost"
	}

	port := os.Getenv("PORT")
	if port == "" {
		port = ":50051"
	}
	address := strings.Join([]string{strings.TrimSuffix(host, ":"), strings.TrimPrefix(port, ":")}, ":")
	log.Println("connecting ...", address)

	// Set up a connection to the server.
	conn, err := grpc.Dial(address, grpc.WithInsecure(), grpc.WithBlock())
	if err != nil {
		log.Fatalf("did not connect: %v", err)
	}
	defer conn.Close()
	c := todorpc.NewTodoBoardClient(conn)

	ctx, cancel := context.WithTimeout(context.Background(), time.Second)
	defer cancel()

	if err := do(ctx, c); err != nil {
		log.Fatalf("could not use: %+v", err)
	}
}

func do(ctx context.Context, c todorpc.TodoBoardClient) error {
	log.Println("start")
	defer log.Println("end")

	{
		r, err := c.List(ctx, &empty.Empty{})
		if err != nil {
			return fmt.Errorf("step 1 %w", err)
		}
		log.Println("<-", r)
	}

	{
		item := &todorpc.Todo{
			Title: "Go to bed",
			Done: true,
		}
		log.Println("->", item)
		if _, err := c.Add(ctx, item); err != nil {
			return fmt.Errorf("step 2 %w", err)
		}
	}

	{
		r, err := c.List(ctx, &empty.Empty{})
		if err != nil {
			return fmt.Errorf("step 3 %w", err)
		}
		log.Println("<-", r)
	}

	return nil
}
