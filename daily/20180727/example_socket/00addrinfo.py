import socket

port = 8080
for host in ["::1", None]:
    for res in socket.getaddrinfo(
        host, port, socket.AF_UNSPEC, socket.SOCK_STREAM, 0, socket.AI_PASSIVE
    ):
        print("@", res)
