d = {}

try:
    d["xxx"]
except KeyError as e:
    print("!", e)


class O:
    def foo(self):
        raise KeyError()


try:
    O().foo()
except KeyError as e:
    print("!", e)
