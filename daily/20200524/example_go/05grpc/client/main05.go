package main

import (
	"context"
	"log"
	"os"
	"strconv"
	"time"

	pb "m/05grpc/internal"

	"google.golang.org/grpc"
)

const (
	address        = "localhost:50051"
	defaultA int64 = 10
	defaultB int64 = 20
)

func main() {
	// Set up a connection to the server.
	conn, err := grpc.Dial(address, grpc.WithInsecure(), grpc.WithBlock())
	if err != nil {
		log.Fatalf("did not connect: %v", err)
	}
	defer conn.Close()
	c := pb.NewArithServiceClient(conn)

	// Contact the server and print out its response.
	a := defaultA
	b := defaultB
	if len(os.Args) > 2 {
		a, _ = strconv.ParseInt(os.Args[1], 10, 0)
		b, _ = strconv.ParseInt(os.Args[2], 10, 0)
	}
	ctx, cancel := context.WithTimeout(context.Background(), time.Second)
	defer cancel()
	log.Printf("Mul: %v * %v", a,b)
	r, err := c.Mul(ctx, &pb.MulRequest{A: a, B: b})
	if err != nil {
		log.Fatalf("could not greet: %v", err)
	}
	log.Printf("Result: %v", r.GetResult())
}
