# -*- coding:utf-8 -*-
import unittest
import unittest.mock as mock
from requests.sessions import Session


class FakeSession(Session):
    def request(self, method, url, **kwargs):
        raise NotImplementedError(method, url, kwargs)


class Tests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls._ms = []

        def request(method, url, **kwargs):
            session = FakeSession()
            return session.request(method=method, url=url, **kwargs)

        cls._ms.append(mock.patch("requests.api.request", new=request))
        cls._ms.append(mock.patch("requests.Session", new=FakeSession))
        cls._ms.append(mock.patch("requests.sessions.Session", new=FakeSession))
        for m in cls._ms:
            m.start()

    @classmethod
    def tearDownClass(cls):
        for m in cls._ms:
            m.stop()

    def test_it(self):
        with self.assertRaises(NotImplementedError):
            import requests
            requests.get("http://example.com")


if __name__ == "__main__":
    unittest.main()
