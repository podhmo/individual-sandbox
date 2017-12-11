import inspect
import unittest.mock as mock
import unittest


def replace_method(m, fn, name=None):
    spec = m.__class__
    typ = type(m)
    name = name or fn.__name__

    assert typ != spec, "{} == {}, maybe spec is not set?".format(typ, spec)

    sig_repr = str(inspect.signature(getattr(spec, name)))
    sig_repr = sig_repr.replace('(self, ', '(')  # xxx work-around
    fn_sig_repr = str(inspect.signature(fn))
    assert sig_repr == fn_sig_repr, "expected {}()'s signature: {}, but {}".format(
        name, sig_repr, fn_sig_repr
    )
    attr = getattr(m, name)
    attr.side_effect = fn


class Ob:
    def add(x, y):
        return x + y


class Tests(unittest.TestCase):
    def test_missing_attribute(self):
        m = mock.Mock(spec=Ob)
        with self.assertRaises(AttributeError):
            m.sub(10, 20)

    def test_attribute(self):
        m = mock.Mock(spec=Ob)
        m.add(10, 20)
        m.add.assert_called_once_with(10, 20)

    def test_spec_is_not_set(self):
        m = mock.Mock()

        def add(x):
            return x + y

        with self.assertRaises(AssertionError):
            replace_method(m, add)

    def test_invalid_args(self):
        m = mock.Mock(spec=Ob)

        def add(x):
            return x + y

        with self.assertRaises(AssertionError):
            replace_method(m, add)

    def test_valid_args(self):
        m = mock.Mock(spec=Ob)

        def add(x, y):
            return x * y

        replace_method(m, add)
        got = m.add(10, 20)
        self.assertEqual(200, got)
        m.add.assert_called_once_with(10, 20)

    def test_invalid_call(self):
        m = mock.Mock(spec=Ob)

        def add(x, y):
            return x * y

        replace_method(m, add)
        with self.assertRaises(TypeError):
            m.add(10)

    def test_replace_fn(self):
        m = mock.Mock(spec=Ob)

        def add(x, y):
            return x * y

        replace_method(m, add)
        with self.assertRaises(TypeError):
            m.add(10)


if __name__ == "__main__":
    unittest.main()
