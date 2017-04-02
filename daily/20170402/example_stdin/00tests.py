import unittest
import contextlib


def get_package_name():
    package = input("input package name:")
    return {"package": package}


class redirect_stdin(contextlib._RedirectStream):
    _stream = "stdin"


class InputTests(unittest.TestCase):
    def _calFUT(self):
        return get_package_name()

    def test_it(self):
        from io import StringIO
        buf = StringIO()
        buf.write("hello\n")
        buf.seek(0)

        with redirect_stdin(buf):
            actual = self._calFUT()

        expected = {"package": "hello"}
        self.assertEqual(actual, expected)


if __name__ == "__main__":
    unittest.main()
