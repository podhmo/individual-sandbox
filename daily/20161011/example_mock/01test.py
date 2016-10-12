import unittest
from unittest import mock


class DummyFoo:
    def __init__(self, name):
        self.name = name

    def save(self):
        self._saved = True
        print("yay")


class Tests(unittest.TestCase):
    @mock.patch("services.Foo", new=DummyFoo)
    def test_it(self):
        from services import foo_service
        foo = foo_service()
        self.assertEqual(foo.name, "name")
        self.assertEqual(foo.desc, "desc")
        self.assertTrue(foo._saved)


if __name__ == "__main__":
    unittest.main()
