# -*- coding:utf-8 -*-
# pip install requests requests-mock
import unittest
import requests
import requests_mock as rm


class Tests(unittest.TestCase):
    def test_it(self):
        with rm.Mocker():
            with self.assertRaises(rm.NoMockAddress):
                print(requests.get("http://example.com").text)

    def test_mocked(self):
        with rm.Mocker() as m:
            m.get('http://example.com', text='resp')
            actual = requests.get("http://example.com").text
        self.assertEqual(actual, "resp")

    def test_mocked_many_paths(self):
        with rm.Mocker() as m:
            m.get('//example.com/foo', text="foo")
            m.get('//example.com/bar', text="bar")

            foo = requests.get("http://example.com/foo").text
            self.assertEqual(foo, "foo")
            bar = requests.get("http://example.com/bar").text
            self.assertEqual(bar, "bar")

    def test_mocked_multiple_responses(self):
        with rm.Mocker() as m:
            m.get('//example.com', [{"text": "foo"}, {"text": "bar"}])

            foo = requests.get("http://example.com/").text
            self.assertEqual(foo, "foo")
            bar = requests.get("http://example.com/").text
            self.assertEqual(bar, "bar")


if __name__ == "__main__":
    unittest.main()
