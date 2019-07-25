import sys


class Wrapper:
    def __init__(self, internal):
        self.internal = internal

    def __getattr__(self, name):
        attr = getattr(self.internal, name)
        if not callable(attr):
            return attr

        def caller(*args, **kwargs):
            print(">>", self.internal.__name__, attr.__name__, args, kwargs)
            v = attr(*args, **kwargs)
            print("<<", self.internal.__name__, v)
            return v

        return caller

    def __call__(self, *args, **kwargs):
        print("@", self.internal, args, kwargs)
        return self.internal(*args, **kwargs)


sys.meta_path = [Wrapper(f) for f in sys.meta_path]  # xxx:
sys.path_hooks = [Wrapper(f) for f in sys.path_hooks]  # xxx:
import re  # noqa
print(re)
