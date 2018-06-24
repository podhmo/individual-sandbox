import socketserver


class Handler(socketserver.StreamRequestHandler):
    def handle(self):
        body = self.rfile.readline()
        print("@", body)
        self.wfile.write(body)


if __name__ == "__main__":
    server = socketserver.ThreadingTCPServer(("localhost", 44444), RequestHandlerClass=Handler)
    server.serve_forever()
