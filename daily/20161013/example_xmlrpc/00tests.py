from magicalimport import import_from_physical_path
import unittest
import threading


class Tests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        server = import_from_physical_path("./00server.py", as_="server")
        cls.t = threading.Thread(daemon=True, target=lambda: server.run_app(server.make_app(7777)))
        cls.t.start()

    def test_it(self):
        import math
        client = import_from_physical_path("./00client.py", as_="client")
        app = client.make_app(7777)

        candidates = [
            ("pow1", pow(2, 3)),
            ("pow2", pow(2, -3)),
            ("pi", math.atan(1) * 4)
        ]

        itr = client.iter_app(app)

        for name, expected in candidates:
            with self.subTest(name=name):
                self.assertEqual(next(itr), expected)

    def test_it2(self):
        client = import_from_physical_path("./00client.py", as_="client")
        app = client.make_app(7777)

        def pow_generator():
            v = None
            while True:
                x, i = yield v
                v = app.pow(x, i)

        itr = pow_generator()
        itr.send(None)
        self.assertEqual(itr.send((10, 3)), 1000)
        self.assertEqual(itr.send((10, -3)), 0.001)


if __name__ == "__main__":
    unittest.main()
