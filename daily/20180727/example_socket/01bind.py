import socket

for af, socktype, proto, canonname, sa in socket.getaddrinfo(
    None, 8080, socket.AF_UNSPEC, socket.SOCK_STREAM, 0, socket.AI_PASSIVE
):
    s = socket.socket(af, socktype, proto)
    s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    print("@", sa)
    s.bind(sa)
    print(socket.getfqdn(s.getsockname()[0]))
    s.close()
