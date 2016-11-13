import unittest


class PreGencodeTests(unittest.TestCase):
    def _makeOne(self, items=None):
        from convert import MiniCodeNormalizer
        return MiniCodeNormalizer()

    def test_it(self):
        path = [
            ('coerce', ('string', ), ('strfmt.Email', )),
            ('coerce', ('strfmt.Email', ), ('pointer', 'strfmt.Email'))
        ]
        target = self._makeOne()
        actual = target.pre_gencode(path)
        self.assertEqual(path, actual)

    def test_array(self):
        path = [
            ("coerce", ("array", "x"), ("array", "y")),
        ]
        target = self._makeOne()
        actual = target.pre_gencode(path)
        expected = [
            ("coerce", ("array", "x"), ("x", )),
            ("coerce", ("x", ), ("y", )),
            ("coerce", ("y", ), ("array", "y")),
        ]
        self.assertEqual(expected, actual)

    def test_array_pointer(self):
        path = [
            ("coerce", ("pointer", "array", "x"), ("pointer", "array", "y")),
        ]
        target = self._makeOne()
        actual = target.pre_gencode(path)
        expected = [
            ("coerce", ("pointer", "array", "x"), ("array", "x", )),
            ("coerce", ("array", "x"), ("x", )),
            ("coerce", ("x", ), ("y", )),
            ("coerce", ("y", ), ("array", "y")),
            ("coerce", ("array", "y", ), ("pointer", "array", "y")),
        ]
        self.assertEqual(expected, actual)

    def test_pointer_array(self):
        path = [
            ("coerce", ("array", "pointer", "x"), ("array", "pointer", "y")),
        ]
        target = self._makeOne()
        actual = target.pre_gencode(path)
        expected = [
            ('coerce', ('array', 'pointer', 'x'), ('pointer', 'x')),
            ('coerce', ('pointer', 'x'), ('pointer', 'y')),
            ('coerce', ('pointer', 'y'), ('array', 'pointer', 'y')),
        ]
        self.assertEqual(expected, actual)

    def test_array_array(self):
        path = [
            ("coerce", ("array", "array", "x"), ("array", "array", "y")),
        ]
        target = self._makeOne()
        actual = target.pre_gencode(path)
        expected = [
            ("coerce", ("array", "array", "x"), ("array", "x", )),
            ("coerce", ("array", "x"), ("x", )),
            ("coerce", ("x", ), ("y", )),
            ("coerce", ("y", ), ("array", "y")),
            ("coerce", ("array", "y", ), ("array", "array", "y")),
        ]
        self.assertEqual(expected, actual)
