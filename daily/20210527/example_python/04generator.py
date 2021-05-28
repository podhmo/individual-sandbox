import time
import sys
import signal
import _cmd as cmd  # for mac


def gen():
    print("++++++++++")
    for i in range(10):
        yield i
        time.sleep(0.5)
    print("*********")


# state
state = []
g = gen()


class Client:
    def receive(self, x):
        print("got", x)
        state.append(x)

    def init(self):
        for x in state:
            print("got from state", x)

    def reset(self):
        global g
        state.clear()
        g = gen()


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
        print(".. exit")
        # sys.exit(0)  # xxx

    do_EOF = do_reconnect
    default = do_reconnect


shell = Shell()


def _handler(signum, frame):
    shell.cmdloop()


signal.signal(signal.SIGINT, _handler)

try:
    while True:
        i = next(g)
        client.receive(i)
except StopIteration:
    pass
finally:
    shell.onecmd("exit")
