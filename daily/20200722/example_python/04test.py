import unittest
import unittest.mock as mock
from app import view


class Tests(unittest.TestCase):
    @mock.patch("app.insert_article")
    def test_ng(self, m):
        m.side_effect = ValueError("bomb")
        with self.assertRaises(ValueError):
            view()

    def test_ok(self):
        view()


if __name__ == "__main__":
    unittest.main()
