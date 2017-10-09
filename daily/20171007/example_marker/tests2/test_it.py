import unittest
from testmarker import mark


@mark("x", skip=True)
class Tests(unittest.TestCase):
    def test_it(self):
        pass


@mark("x", skip=True)
class Tests2(unittest.TestCase):
    def test_it(self):
        pass
