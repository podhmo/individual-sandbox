package main

import (
	"context"
	"log"
	"net"

	"google.golang.org/grpc"
	pb "m/05grpc/internal"
)

const (
	port = ":50051"
)

// server is used to implement internal.ArithService.
type server struct {
	pb.UnimplementedArithServiceServer
}

// Mul implements internal.ArithService
func (s *server) Mul(ctx context.Context, in *pb.MulRequest) (*pb.MulResponse, error) {
	log.Printf("Received: a=%v, b=%v", in.GetA(), in.GetB())
	return &pb.MulResponse{Result: in.GetA() + in.GetB()}, nil
}

func main() {
	lis, err := net.Listen("tcp", port)
	if err != nil {
		log.Fatalf("failed to listen: %v", err)
	}
	s := grpc.NewServer()
	pb.RegisterArithServiceServer(s, &server{})
	if err := s.Serve(lis); err != nil {
		log.Fatalf("failed to serve: %v", err)
	}
}

