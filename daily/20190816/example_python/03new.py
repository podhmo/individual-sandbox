import time


class A:
    def __init__(self, x):
        self.x = x

    def __new__(cls, x):
        return super().__new__(cls)


class B:
    def __init__(self, x):
        print("start init")
        self.x = x
        print("end init")

    def __new__(cls, x):
        print("start new")
        ob = super().__new__(cls)
        print("end new")
        return ob


a = A("x")
(
    a.__class__,
    a.x,
    getattr(a, "y", None),
)  # => (<class 'exec.A'>, 'x', None)
b = B("x")
(
    b.__class__,
    b.x,
    getattr(b, "y", None),
)  # => (<class 'exec.B'>, 'x', None)
# -- stdout --------------------
# >> start new
# >> end new
# >> start init
# >> end init
