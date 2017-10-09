import unittest
from testmarker import mark


@mark.a
class Test0(unittest.TestCase):
    def test_it(self):
        pass


class Test1(unittest.TestCase):
    @mark.a
    def test_it(self):
        pass

    @mark.b
    def test_it2(self):
        pass


class Test2(unittest.TestCase):
    def test_it(self):
        pass
