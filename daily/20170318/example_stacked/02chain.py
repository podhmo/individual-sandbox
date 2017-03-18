import contextlib
from collections import deque


class Stacked(object):
    def __init__(self):
        self.stack = deque()

    def push(self, frame):
        self.stack.append(frame)

    def pop(self):
        self.stack.pop()

    def __getattr__(self, name):
        for d in reversed(self.stack):
            if name in d:
                return d[name]
        raise AttributeError(name)

    @contextlib.contextmanager
    def scope(self, frame):
        self.push(frame)
        try:
            yield frame
        finally:
            self.pop()


stacked = Stacked()
stacked.push({"top": "hai"})
stacked.push({"middle": "hoi"})
stacked.push({"bottom": "yay"})
print(stacked.top)
print(stacked.middle)
print(stacked.bottom)


stacked = Stacked()
with stacked.scope({"top": "hai"}):
    with stacked.scope({"middle": "hoi"}):
        with stacked.scope({"bottom": "yay"}):
            print(stacked.top)
            print(stacked.middle)
            print(stacked.bottom)
