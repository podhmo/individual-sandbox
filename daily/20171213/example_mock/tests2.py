import unittest
import unittest.mock as mock
import inspect


class Ob:
    def hello(self, name):
        return "hello:{}".format(name)


def replace_method_with_signature_check(m, fn, name=None):
    """mock中のmethodをsignatureを考慮して書き換えるもの"""
    spec = m.__class__
    typ = type(m)
    name = name or fn.__name__

    assert typ != spec, "{} == {}, maybe spec is not set?".format(typ, spec)

    sig_repr = str(inspect.signature(getattr(spec, name)))
    sig_repr = sig_repr.replace('(self, ', '(')  # xxx work-around
    fn_sig_repr = str(inspect.signature(fn))
    assert sig_repr == fn_sig_repr, "expected {}()'s signature: {}, but {}".format(name, sig_repr, fn_sig_repr)
    attr = getattr(m, name)
    attr.side_effect = fn


class Tests(unittest.TestCase):
    def test_mismatch_signature(self):
        # signatureの異なるmethodに指定しても通ってしまう
        m = mock.Mock(spec_set=Ob)

        # Ob.hello()に対して引数が不足した定義
        def hello():
            return "*replaced*"

        replace_method_with_signature_check(m, hello)
        got = m.hello()
        self.assertEqual(got, "*replaced*")


if __name__ == "__main__":
    unittest.main()
