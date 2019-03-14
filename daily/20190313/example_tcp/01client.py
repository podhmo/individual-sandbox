import socket
from cmd import Cmd


host = "localhost"
port = 44444


class Prompt(Cmd):
    prompt = "> "

    def do_exit(self, inp):
        return True

    def do_send(self, line):
        with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:
            sock.connect((host, port))
            print(f"send: {line}")
            sock.sendall(bytes(line + "\n", "utf-8"))
            print(f"recv: {sock.recv(4096)}")

    def default(self, line):
        return self.do_send(line)


# Prompt().onecmd("send xxx")
Prompt().cmdloop()
