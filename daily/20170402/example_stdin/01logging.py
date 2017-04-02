import unittest
import contextlib


def foo():
    print("foo")


class InputTests(unittest.TestCase):
    def _calFUT(self):
        return foo()

    def test_it(self):
        from io import StringIO
        buf = StringIO()

        with contextlib.redirect_stdout(buf):
            self._calFUT()
        actual = buf.getvalue()
        self.assertEqual(actual, "foo\n")


if __name__ == "__main__":
    unittest.main()
