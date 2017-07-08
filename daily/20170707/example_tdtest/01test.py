import unittest
from collections import namedtuple


class Tests(unittest.TestCase):
    def _callFUT(self, x, y):
        from add import add
        return add(x, y)

    def test_it(self):
        C = namedtuple("C", "msg x y expected")
        candidates = [
            C(msg="with positive", x=10, y=10, expected=20),
            C(msg="with zero", x=10, y=0, expected=10),
            C(msg="with negative", x=10, y=-10, expected=0),
            C(
                msg="with biiiiiiig",
                x=10,
                y=10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000,
                expected=10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010,
            ),
        ]
        for c in candidates:
            with self.subTest(msg=c.msg):
                actual = self._callFUT(c.x, c.y)
                self.assertEqual(actual, c.expected + 1)


if __name__ == "__main__":
    unittest.main()
