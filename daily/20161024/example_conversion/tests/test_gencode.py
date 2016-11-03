import unittest


class Tests(unittest.TestCase):
    def _makeOne(self, items=None):
        from convert import MiniCodeGenerator
        from convert import TypeMappingResolver
        return MiniCodeGenerator(TypeMappingResolver(items or []))

    def test_it(self):
        target = self._makeOne()
        actual = target.gencode(["string"], ["string"])
        expected = []
        self.assertEqual(expected, actual)

    def test_src_pointer(self):
        target = self._makeOne()
        actual = target.gencode(["pointer", "string"], ["string"])
        expected = [("deref",)]
        self.assertEqual(expected, actual)

    def test_dst_pointer(self):
        target = self._makeOne()
        actual = target.gencode(["string"], ["pointer", "string"])
        expected = [("ref",)]
        self.assertEqual(expected, actual)

    def test_same_with_pointer(self):
        target = self._makeOne()
        actual = target.gencode(["pointer", "string"], ["pointer", "string"])
        expected = []
        self.assertEqual(expected, actual)

    def test_src_pointer_pointer(self):
        target = self._makeOne()
        actual = target.gencode(["pointer", "pointer", "string"], ["pointer", "string"])
        expected = [("deref",)]
        self.assertEqual(expected, actual)

    def test_src_pointer_pointer2(self):
        target = self._makeOne()
        actual = target.gencode(["pointer", "pointer", "string"], ["string"])
        expected = [("deref",), ("deref",)]
        self.assertEqual(expected, actual)

    def test_cast(self):
        target = self._makeOne({"string": "X"}.items())
        actual = target.gencode(["string"], ["X"])
        expected = [("coerce", "string", "X")]
        self.assertEqual(expected, actual)

    def test_cast2(self):
        target = self._makeOne([("string", "X"), ("string", "Y")])
        actual = target.gencode(["Y"], ["X"])
        expected = [("coerce", "Y", "string"), ("coerce", "string", "X")]
        self.assertEqual(expected, actual)

    def test_cast3(self):
        target = self._makeOne([("string", "X"), ("string", "Y")])
        actual = target.gencode(["pointer", "pointer", "Y"], ["pointer", "X"])
        expected = [("deref",), ("deref", ), ("coerce", "Y", "string"), ("coerce", "string", "X"), ("ref", )]
        self.assertEqual(expected, actual)

    def test_cast4(self):
        target = self._makeOne([(("pointer", "string"), "PX"), ("string", "Y")])
        actual = target.gencode("Y", "PX")
        expected = [('coerce', 'Y', 'string'), ('ref',), ('coerce', ('pointer', 'string'), 'PX')]
        self.assertEqual(expected, actual)

    def test_cast5(self):
        target = self._makeOne([(("pointer", "string"), "PX"), ("string", "Y")])
        actual = target.gencode("PX", "Y")
        expected = [('coerce', 'PX', ('pointer', 'string')), ('deref',), ('coerce', 'string', 'Y')]
        self.assertEqual(expected, actual)
