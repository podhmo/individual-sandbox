import time
import sys
import _cmd as cmd  # for mac


# state
i = 0
state = []


class Client:
    def receive(self, x):
        print("got", x)
        state.append(x)

    def init(self):
        for x in state:
            print("got from state", x)

    def reset(self):
        global i
        i = 0
        state.clear()


client = Client()


class Shell(cmd.Cmd):
    def do_reconnect(self, arg):
        print("reconnect ...", file=sys.stderr)
        return True

    def do_connect(self, arg):
        print("connect ...", file=sys.stderr)
        client.init()
        return True

    def do_reset(self, arg):
        print("reset ...", file=sys.stderr)
        client.reset()
        return True

    def do_exit(self, arg):
        sys.exit(0)  # xxx

    do_EOF = do_reconnect
    default = do_reconnect


shell = Shell()
try:
    while True:
        try:
            while True:
                client.receive(i)
                time.sleep(0.5)
                i += 1
        except KeyboardInterrupt:
            shell.cmdloop()
except KeyboardInterrupt:
    shell.onecmd("exit")
