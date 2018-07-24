class A:
    def __init__(self, x):
        self.x = x


class B:
    x = "foo"

    def __init__(self, x=None):
        if x is not None:
            self.x = x


print(A("foo").__dict__)  # {'x': 'foo'}
print(B("foo").__dict__)  # {'x': 'foo'}
print(B().__dict__)  # {}
