from handofcats import as_command
import grpc
import helloworld_pb2
import helloworld_pb2_grpc


@as_command
def run():
    with grpc.insecure_channel("localhost:50051") as channel:
        stub = helloworld_pb2_grpc.GreeterStub(channel)
        response = stub.SayHello(helloworld_pb2.HelloRequest(name="you"))
        print(f"Greeter client received: {response.message}")
        response = stub.SayHelloAgain(helloworld_pb2.HelloRequest(name="you"))
        print(f"Greeter client received: {response.message}")
