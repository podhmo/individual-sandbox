import unittest
from unittest import mock


class Tests(unittest.TestCase):
    @mock.patch("services.Foo")
    def test_it(self, MockFoo):
        from services import foo_service
        foo = foo_service()
        MockFoo.assert_called_once_with("name")

        self.assertEqual(foo.desc, "desc")
        foo.save.assert_called_once_with()

if __name__ == "__main__":
    unittest.main()
