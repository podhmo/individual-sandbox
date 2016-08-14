import unittest


class Tests(unittest.TestCase):
    def test_it(self):
        from foo import message
        result = message("byebye")
        self.assertEqual(result, "foo: byebye")
