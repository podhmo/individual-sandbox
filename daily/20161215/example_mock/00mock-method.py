import unittest.mock as mock


class Foo(object):
    def hello(self, v):
        raise Exception("foo")

foo = Foo()
foo.hello = mock.Mock()
foo.hello.return_value = "yup"
assert foo.hello("bar") == "yup"
foo.hello.assert_called_once_with("bar")

try:
    Foo().hello("bar")
except Exception as e:
    print(e)
