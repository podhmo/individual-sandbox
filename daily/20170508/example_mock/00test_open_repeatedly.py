import unittest
from unittest import mock


class TestOpenRepeatedly(unittest.TestCase):
    def _callFUT(self, path):
        import open_repeatedly
        return open_repeatedly.open_repeatedly(path)

    @mock.patch('open_repeatedly.open')
    def test_open(self, m):
        ret = object()
        m.return_value = ret
        path = '/path/to/test.txt'

        f = self._callFUT(path)
        self.assertEqual(f, ret)
        m.assert_called_with(path)


if __name__ == "__main__":
    import logging
    logging.basicConfig(level=logging.INFO)
    unittest.main()
