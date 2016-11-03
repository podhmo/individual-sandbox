import unittest


class Tests(unittest.TestCase):
    def _makeOne(self, coerce_map=None):
        from convert import MiniCodeGenerator
        return MiniCodeGenerator(coerce_map)

    def test_it(self):
        target = self._makeOne()
        actual = target.gencode(["string"], ["string"])
        expected = []
        self.assertEqual(expected, actual)

    def test_src_pointer(self):
        target = self._makeOne()
        actual = target.gencode(["string", "pointer"], ["string"])
        expected = [["deref"]]
        self.assertEqual(expected, actual)

    def test_dst_pointer(self):
        target = self._makeOne()
        actual = target.gencode(["string"], ["string", "pointer"])
        expected = [["ref"]]
        self.assertEqual(expected, actual)

    def test_same_with_pointer(self):
        target = self._makeOne()
        actual = target.gencode(["string", "pointer"], ["string", "pointer"])
        expected = []
        self.assertEqual(expected, actual)

    def test_src_pointer_pointer(self):
        target = self._makeOne()
        actual = target.gencode(["string", "pointer", "pointer"], ["string", "pointer"])
        expected = [["deref"]]
        self.assertEqual(expected, actual)

    def test_src_pointer_pointer2(self):
        target = self._makeOne()
        actual = target.gencode(["string", "pointer", "pointer"], ["string"])
        expected = [["deref"], ["deref"]]
        self.assertEqual(expected, actual)

    def test_cast(self):
        target = self._makeOne({"string": "X"})
        actual = target.gencode(["string"], ["X"])
        expected = [["coerce", "string", "X"]]
        self.assertEqual(expected, actual)
