import sys
import socket

fname = sys.argv[1]
PORT = 12345
CHUNK_SIZE = 8 * 1024

server_socket = socket.socket()
server_socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEPORT, 1)
server_socket.bind(('localhost', PORT))
server_socket.listen(5)  # backlog=5

while True:
    client_socket, addr = server_socket.accept()
    with client_socket:
        with open(fname, 'rb') as f:
            print(client_socket.sendfile(f, 0))
