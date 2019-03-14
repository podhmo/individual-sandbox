import socket
from pyls_jsonrpc.streams import JsonRpcStreamWriter
from cmd import Cmd


host = "localhost"
port = 44444


class Prompt(Cmd):
    prompt = "> "

    def do_exit(self, inp):
        return True

    def do_send(self, line):
        return send(line)

    def default(self, line):
        return self.do_send(line)


def send(line):
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:
        sock.connect((host, port))
        print(f"send: {line}")
        writer = JsonRpcStreamWriter(sock.makefile("wb"))
        writer.write(line)
        print(f"recv: {sock.recv(4096)}")


# Prompt().onecmd("send xxx")
Prompt().cmdloop()
