import unittest


class Tests(unittest.TestCase):
    def _callFUT(self, x, y):
        from add import add
        return add(x, y)

    def test_with_positive(self):
        actual = self._callFUT(10, 10)
        self.assertEqual(actual, 20)

    def test_with_zero(self):
        actual = self._callFUT(10, 0)
        self.assertEqual(actual, 10)

    def test_with_negative(self):
        actual = self._callFUT(10, -10)
        self.assertEqual(actual, 0)

    def test_with_biiiiiiiiiig(self):
        actual = self._callFUT(
            10,
            10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000,
        )
        self.assertEqual(
            actual,
            10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010,
        )


if __name__ == "__main__":
    unittest.main()
