import time
import sys
import signal

# import _cmd as cmd  # for mac
import cmd


def generate_event_stream():
    for i in range(10):
        yield i  # todo: event
        state.append(i)
        time.sleep(0.5)
    yield None  # end of stream


def get_state():
    global state
    return state


# state
state = []
onetime = False


class ShutdownException(Exception):
    pass


class Shell(cmd.Cmd):
    prompt = "(Browser) "

    def do_connect(self, arg):
        print("connect ...", file=sys.stderr)
        for x in get_state():
            print("got from state", x)
        return True

    def do_reconnect(self, arg):
        print("reconnect ...", file=sys.stderr)
        return True

    def do_close(self, arg):
        raise ShutdownException(arg)


shell = Shell()


def _handler(signum, frame):
    try:
        shell.cmdloop()
    except RuntimeError as ex:
        print(ex, file=sys.stderr)
        sys.exit(1)


# for Ctrl + c
signal.signal(signal.SIGINT, _handler)


def event_handler(i):
    if i is None:
        # shell.do_close("")
        return

    print("got", i)


shell.do_connect("")
try:
    while True:
        try:
            g = generate_event_stream()
            for i in g:
                event_handler(i)
            shell.do_reconnect("")
            time.sleep(0.7)
            state.clear()
        except ShutdownException:
            break
finally:
    print("exit..")
