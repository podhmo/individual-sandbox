import unittest


def add(x, y):
    return x + y


class Tests(unittest.TestCase):
    def test_it(self):
        got = add(10, 20)
        expected = 30
        self.assertEqual(got, expected)


if __name__ == "__main__":
    unittest.main()
