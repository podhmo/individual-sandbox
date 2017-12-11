import unittest.mock as mock
import unittest


class Ob:
    def add(x, y):
        return x + y


class Tests(unittest.TestCase):
    def test_missing_attribute(self):
        m = mock.Mock(spec=Ob)
        with self.assertRaises(AttributeError):
            m.sub(10, 20)

    def test_attribute(self):
        m = mock.Mock(spec=Ob)
        m.add(10, 20)
        m.add.assert_called_once_with(10, 20)

    def test_invalid_args(self):
        m = mock.Mock(spec=Ob)
        m.add(10)  # hmm

    def test_invalid_args__fn(self):
        def add(x, y):
            return x + y

        madd = mock.Mock(spec=add)
        madd(10)  # hmm


if __name__ == "__main__":
    unittest.main()
