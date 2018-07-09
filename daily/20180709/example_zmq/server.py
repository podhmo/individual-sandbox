import zmq


def main():
    context = zmq.Context()
    sock = context.socket(zmq.REQ)
    sock.connect("tcp://localhost:55555")
    for i in range(5):
        print(i)
        sock.send_string(f"hello {i}")
        message = sock.recv_string()
        print(f"reply {i}: {message}")


if __name__ == "__main__":
    main()
