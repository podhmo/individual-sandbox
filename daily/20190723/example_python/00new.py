import sys


class Proxy:
    def __init__(self, internal):
        self.internal = internal

    def __getattr__(self, name):
        return getattr(self.internal, name)


class Finder:
    def __new__(cls, internal):
        if isinstance(internal, Finder):
            return Proxy(internal)
        else:
            return super().__new__(cls)

    def __init__(self, internal):
        print("@")
        self.internal = internal

    def __getattr__(self, name):
        attr = getattr(self.internal, name)
        if not callable(attr):
            return attr

        def caller(*args, **kwargs):
            print(attr, args, kwargs, file=sys.stderr)
            return attr(*args, **kwargs)

        return caller


f = Finder({})
print(f, f.internal)
f2 = Finder(f)
print(f2, f2.internal)
