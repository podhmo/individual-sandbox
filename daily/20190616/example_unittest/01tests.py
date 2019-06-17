import unittest
from testing2 import AlwaysFailMixin


class Tests(AlwaysFailMixin, unittest.TestCase):
    def test_it(self):
        self.assert_always_fail()


if __name__ == "__main__":
    unittest.main()
