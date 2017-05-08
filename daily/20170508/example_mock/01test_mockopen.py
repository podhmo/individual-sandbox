import unittest
from unittest import mock


def first_line():
    with open("./foo.txt") as rf:
        # read() and readlines() and readline(), but iter(rf) is not supported
        return rf.readline()


class Tests(unittest.TestCase):
    def _callFUT(self):
        return first_line()

    def test_it(self):
        with mock.patch.object(__builtins__, "open", new_callable=mock.mock_open, read_data="foo\nbar") as m:
            r = self._callFUT()
            m.assert_called_once_with("./foo.txt")
            self.assertEqual(r, "foo\n")

    def test_it2(self):
        with mock.patch("builtins.open", new_callable=mock.mock_open, read_data="foo\nbar") as m:
            r = self._callFUT()
            m.assert_called_once_with("./foo.txt")
            self.assertEqual(r, "foo\n")


if __name__ == "__main__":
    unittest.main()
