import unittest.mock as mock


class Foo(object):
    def hello(self, v):
        raise Exception("foo")

foo = Foo()
with mock.patch.object(foo, "hello") as hello:
    hello.return_value = "yup"

    assert foo.hello("bar") == "yup"
    hello.assert_called_once_with("bar")

try:
    Foo().hello("bar")
except Exception as e:
    print(e)
