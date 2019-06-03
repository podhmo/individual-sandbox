import unittest.mock as mock


class A:
    def __getitem__(self, k):
        raise Exception(k)

    def f(self, k):
        raise Exception(k)


def f():
    a = A()
    a.f("x")
    return 10


try:
    f()
except Exception as e:
    print(e)

try:
    with mock.patch(f"{__name__}.A.f", return_value=10) as mock_method:
        print(f())
except Exception as e:
    print("m", e)

