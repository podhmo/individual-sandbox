import socketserver

addr = ""
port = 44444


class Handler(socketserver.StreamRequestHandler):
    def handle(self):
        self.data = self.rfile.readline().strip()
        print(f"got: {self.data}")
        self.wfile.write(bytes(self.data.upper()))


addr = "localhost"
port = 44444
with socketserver.TCPServer((addr, port), Handler) as s:
    s.allow_reuse_address = True
    s.serve_forever()

# or handle_request
