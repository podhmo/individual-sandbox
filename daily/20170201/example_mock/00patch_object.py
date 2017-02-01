import unittest.mock as mock


class Foo(object):
    def foo(self):
        return "foo"

foo = Foo()
print(foo.foo())  # "foo"

with mock.patch.object(foo, "foo") as m:
    m.side_effect = Exception("oops")
    try:
        print(foo.foo())
    except Exception as e:
        print(e)  # oops

print(foo.foo())  # foo
