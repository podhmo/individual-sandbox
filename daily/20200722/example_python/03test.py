import unittest
import unittest.mock as mock


class Tests(unittest.TestCase):
    @mock.patch("app.insert_article")
    def test_ng(self, m):
        m.side_effect = ValueError("bomb")
        from app import view

        with self.assertRaises(ValueError):
            view()

    def test_ok(self):
        from app import view

        view()


if __name__ == "__main__":
    unittest.main()
