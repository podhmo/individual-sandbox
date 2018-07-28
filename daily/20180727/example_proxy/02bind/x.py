import socket

s = socket.socket(socket.AF_INET6, socket.SOCK_STREAM)
# s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
# s.bind(('::1', 8080))
s.bind(('127.0.0.1', 8080))
print(s.getsockname())
print(socket.getfqdn(s.getsockname()[0]))
