import unittest
import unittest.mock as mock
import textwrap
import contextlib
from io import StringIO


def add():
    print("X + Yを計算します")
    x = input("X")
    y = input("Y")
    print(f"{x} + {y} = {int(x) + int(y)} です")


class Tests(unittest.TestCase):
    def test_it(self):
        o = StringIO()
        with mock.patch("builtins.input") as minput:
            minput.side_effect = [10, 20]
            with contextlib.redirect_stdout(o):
                add()

        got = o.getvalue()
        expected = textwrap.dedent("""
        X + Yを計算します
        10 + 20 = 30 です
        """)
        self.assertEqual(got.strip(), expected.strip())


if __name__ == "__main__":
    unittest.main()
