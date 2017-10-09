import unittest
from marking import markers


@markers.a
class Test0(unittest.TestCase):
    def test_it(self):
        pass


@markers.b
class Test1(unittest.TestCase):
    def test_it(self):
        pass


@markers.c
class Test2(unittest.TestCase):
    def test_it(self):
        pass


@markers.d
class Test3(unittest.TestCase):
    def test_it(self):
        pass


@markers.e
class Test4(unittest.TestCase):
    def test_it(self):
        pass
