# -*- coding:utf-8 -*-
import unittest
import urllib.request as r


class DummyHandler(r.HTTPHandler):
    def http_open(self, req):
        raise NotImplementedError("req: {}".format(req.full_url))


class Tests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        r.install_opener(r.build_opener(DummyHandler))

    @classmethod
    def tearDownClass(cls):
        r.install_opener(None)

    def test_it(self):
        with self.assertRaises(NotImplementedError):
            r.urlopen("http://example.com")


if __name__ == "__main__":
    unittest.main()
