import time
import sys
import signal
import _cmd as cmd  # for mac


def gen():
    print("++++++++++")
    for i in range(10):
        yield i  # todo: event
        time.sleep(0.5)
    yield None  # end of stream
    print("*********")


# state
state = []
g = gen()
onetime = False


class ShutdownException(Exception):
    pass


class BrowserLike(cmd.Cmd):
    def do_connect(self, arg):
        print("connect ...", file=sys.stderr)
        for x in state:
            print("got from state", x)
        return True

    def do_reconnect(self, arg):
        print("reconnect ...", file=sys.stderr)
        return True

    def do_close(self, arg):
        raise ShutdownException(arg)


b = BrowserLike()


def _handler(signum, frame):
    b.cmdloop()


signal.signal(signal.SIGINT, _handler)


def event_handler(i):
    # global onetime
    # if i is None:
    #     onetime = True
    #     return

    print("got", i)
    state.append(i)


b.onecmd("connect")
try:
    while True:
        try:
            while True:
                i = next(g)
                event_handler(i)
                if onetime:
                    break
        except ShutdownException:
            break
        except StopIteration:
            if onetime:
                break

            b.onecmd("reconnect")
            time.sleep(0.7)
            state.clear()
            g = gen()

finally:
    print("exit..")
