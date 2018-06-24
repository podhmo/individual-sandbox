import socket
import sys

HOST, PORT = "localhost", 44444
data = " ".join(sys.argv[1:])

with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:
    sock.connect((HOST, PORT))
    sock.sendall(bytes(data + "\n", "utf-8"))
    print(f"Sent:     {data}")
    received = str(sock.recv(1024), "utf-8")
    print(f"Received: {received}")
    received = str(sock.recv(1024), "utf-8")
    print(f"Received: {received}")
