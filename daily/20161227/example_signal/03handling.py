import sys
import time
import signal
import contextlib
import random
from collections import deque


class GracefulStopManager(object):
    def __init__(self):
        self.signals = [signal.SIGINT, signal.SIGTERM, signal.SIGQUIT, signal.SIGHUP]
        self.attached = False
        self.callback_stack = deque(maxlen=10)

    def on_stop(self, signum, frame):
        for cb in reversed(self.callback_stack):
            # ここでexception handlingするべきか微妙
            cb(signum, frame)
        sys.exit(-1)

    @contextlib.contextmanager
    def attach(self, callback):
        if not self.attached:
            for s in self.signals:
                signal.signal(s, self.on_stop)
            self.attached = True
        self.callback_stack.append(callback)
        yield
        self.callback_stack.pop()


_MANAGER = None


# not thread safe
def get_graceful_stop_manager(factory=GracefulStopManager):
    global _MANAGER
    if _MANAGER is None:
        set_graceful_stop_manager(GracefulStopManager())
    return _MANAGER


def set_graceful_stop_manager(manager):
    global _MANAGER
    previous = _MANAGER
    _MANAGER = GracefulStopManager()
    return previous


def inner_loop():
    i = 0

    while True:
        m = get_graceful_stop_manager()
        x = random.random()
        print(x)

        def on_stop(*args, **kwargs):
            print("stop on", i, x)

        with m.attach(on_stop):
            time.sleep(0.5)
            i += 1


def loop():
    def on_stop_parent(*args, **kwargs):
        print("stop parent")

    m = get_graceful_stop_manager()
    with m.attach(on_stop_parent):
        inner_loop()


if __name__ == "__main__":
    loop()
