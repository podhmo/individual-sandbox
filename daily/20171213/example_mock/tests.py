import unittest
import unittest.mock as mock


class Ob:
    def hello(self, name):
        return "hello:{}".format(name)


class Tests(unittest.TestCase):
    def test_attr_missing(self):
        # 属性無しはOK
        m = mock.Mock(spec_set=Ob)
        with self.assertRaises(AttributeError):
            m.bye()

    def test_mismatch_signature(self):
        # signatureの異なるmethodに指定しても通ってしまう
        m = mock.Mock(spec_set=Ob)
        m.hello.side_effect = lambda: "*replaced*"
        # 本当は hello("foo") などでなければダメ
        got = m.hello()
        self.assertEqual(got, "*replaced*")


if __name__ == "__main__":
    unittest.main()
