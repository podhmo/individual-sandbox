import unittest
try:
    import mock # python 2.7
except ImportError:
    from unittest import mock


class MockFactory:
    def __init__(self):
        self.mocks = []

    def __call__(self, **kwargs):
        m = mock.Mock()
        for k, v in kwargs.items():
            setattr(m, k, v)
        self.mocks.append(m)
        return m


class Tests(unittest.TestCase):
    @mock.patch("services.Foo", new_callable=MockFactory)
    def test_foo_it1(self, mf):
        from services import foo_service1
        foo_service1()
        foo = mf.mocks[0]
        self.assertEqual(foo.name, "foo name")
        self.assertEqual(foo.desc, "foo desc")
        foo.save.assert_called_once_with()

    @mock.patch("services.Foo", new_callable=MockFactory)
    def test_foo_it2(self, mf):
        from services import foo_service2
        foo_service2()
        foo = mf.mocks[0]
        self.assertEqual(foo.name, "foo name")
        self.assertEqual(foo.desc, "foo desc")
        foo.save.assert_called_once_with()

    @mock.patch("services.Foo", new_callable=MockFactory)
    def test_foo_it3(self, mf):
        from services import foo_service3
        foo_service3()
        foo = mf.mocks[0]
        self.assertEqual(foo.name, "foo name")
        self.assertEqual(foo.desc, "foo desc")
        foo.save.assert_called_once_with()


if __name__ == "__main__":
    unittest.main()
