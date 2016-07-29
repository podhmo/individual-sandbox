import unittest


class Tests(unittest.TestCase):
    def test_it(self):
        from foo import message
        result = message("hello")
        self.assertEqual(result, "foo: hello")
