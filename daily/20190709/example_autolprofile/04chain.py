import unittest


def chain(xs, *, b=None, a=None):
    for x in xs:
        yield x


class Tests(unittest.TestCase):
    def _callFUT(self, xs):
        return list(chain(xs))

    def test_it(self):
        cases = [([1], [1]), ([1, 1], [1]), ([1, 2, 3], [1, 2, 3]), ([1, 0], [1])]
        for input, want in cases:
            with self.subTest(input=input, want=want):
                got = self._callFUT(input)
                self.assertEqual(got, want)


unittest.main()
