import copy
import unittest
import unittest.mock as mock


class A:
    name = "a"


class B:
    name = "b"


def build_message(has_name, fmt):
    return fmt.format(name=has_name.name)


def hello_world(ob):
    fmt = "{}: hello world."
    return build_message(copy.copy(ob), fmt)


class Test(unittest.TestCase):
    def test_it__failure(self):
        with mock.patch("{}.build_message".format(__name__)) as m:
            a = A()
            hello_world(a)
            # this is not success, cause in hello_world() function, copied by copy.copy().
            with self.assertRaises(AssertionError):
                m.assert_called_once_with(a, "{}: hello world.")

    def test_it(self):
        class Dummy:
            def __eq__(self, other):
                return isinstance(other, A)

        with mock.patch("{}.build_message".format(__name__)) as m:
            a = A()
            hello_world(a)
            # this is ok.
            m.assert_called_with(Dummy(), "{}: hello world.")

if __name__ == "__main__":
    unittest.main()
