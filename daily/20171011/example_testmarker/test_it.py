import unittest
from testmarker import mark

@mark.this
class BrokenTests(unittest.TestCase):
    def test_it(self):
        print("1"
