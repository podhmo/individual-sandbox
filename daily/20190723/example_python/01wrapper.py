import sys


class Wrapper:
    def __init__(self, internal):
        self.internal = internal

    def __getattr__(self, name):
        attr = getattr(self.internal, name)
        if not callable(attr):
            return attr

        def caller(*args, **kwargs):
            print(self, attr.__name__, args, kwargs, file=sys.stderr)
            return attr(*args, **kwargs)

        return caller


w = Wrapper({})
print(w.update({"x": "y"}))
# <__main__.Wrapper object at 0x7f32524715f8> update ({'x': 'y'},) {}
