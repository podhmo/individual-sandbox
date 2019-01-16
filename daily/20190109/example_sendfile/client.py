import socket

CHUNK_SIZE = 8 * 1024

with socket.socket() as sock:
    sock.connect(('127.0.0.1', 12345))
    chunk = sock.recv(CHUNK_SIZE)

    while chunk:
        chunk = sock.recv(CHUNK_SIZE)
