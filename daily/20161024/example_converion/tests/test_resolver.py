import unittest


class Tests(unittest.TestCase):
    def _makeOne(self, items=None):
        from convert import TypeMappingResolver
        return TypeMappingResolver(items or [])

    def test_it(self):
        target = self._makeOne()
        actual = target.resolve(src="string", dst="string")
        expected = []
        self.assertEqual(expected, actual)

    def test_string_to_x(self):
        # string => X (string -> X)
        target = self._makeOne([("string", "X")])
        actual = target.resolve(src="string", dst="X")
        expected = [["coerce", "string", "X"]]
        self.assertEqual(expected, actual)

    def test_x_to_string(self):
        # X (string -> X) => string
        target = self._makeOne([("string", "X")])
        actual = target.resolve(src="X", dst="string")
        expected = [["coerce", "X", "string"]]
        self.assertEqual(expected, actual)

    def test_x_to_y(self):
        # X (string -> X) => Y (string -> Y)
        target = self._makeOne([("string", "X"), ("string", "Y")])
        actual = target.resolve(src="X", dst="Y")
        expected = [["coerce", "X", "string"], ["coerce", "string", "Y"]]
        self.assertEqual(expected, actual)

    def test_string_to_y(self):
        # string => Y (string -> X; X => Y)
        target = self._makeOne([("string", "X"), ("X", "Y")])
        actual = target.resolve(src="string", dst="Y")
        expected = [["coerce", "string", "X"], ["coerce", "X", "Y"]]
        self.assertEqual(expected, actual)

    def test_x2_to_y1(self):
        # X2 (string -> X0; X0 -> X1 -> X1 -> X2) => Y1 (string -> Y0; Y0 -> Y1 -> Y1 -> Y2)
        target = self._makeOne([
            ("string", "X0"), ("X0", "X1"), ("X1", "X2"),
            ("string", "A0"), ("A0", "A1"), ("A1", "A2"),
            ("string", "B0"), ("B0", "B1"), ("B1", "B2"),
            ("string", "C0"), ("C0", "C1"), ("C1", "C2"),
            ("string", "D0"), ("D0", "D1"), ("D1", "D2"),
            ("string", "Y0"), ("Y0", "Y1"), ("Y1", "Y2"),
        ])
        actual = target.resolve(src="X2", dst="Y1")
        expected = [
            ['coerce', 'X2', 'X1'],
            ['coerce', 'X1', 'X0'],
            ['coerce', 'X0', 'string'],
            ['coerce', 'string', 'Y0'],
            ['coerce', 'Y0', 'Y1']
        ]
        self.assertEqual(expected, actual)

    def test_x2_to_y1__shortest(self):
        # X2 (string -> X0; X0 -> X1 -> X1 -> X2) => Y1 (string -> Y0; Y0 -> Y1 -> Y1 -> Y2)
        target = self._makeOne([
            ("string", "X0"), ("X0", "X1"), ("X1", "X2"),
            ("string", "A0"), ("A0", "A1"), ("A1", "A2"),
            ("string", "B0"), ("B0", "B1"), ("B1", "B2"),
            ("string", "C0"), ("C0", "C1"), ("C1", "C2"),
            ("string", "D0"), ("D0", "D1"), ("D1", "D2"),
            ("string", "Y0"), ("Y0", "Y1"), ("Y1", "Y2"),
        ])
        actual = target.resolve(src="X2", dst="Y1")
        expected = [
            ['coerce', 'X2', 'X1'],
            ['coerce', 'X1', 'X0'],
            ['coerce', 'X0', 'string'],
            ['coerce', 'string', 'Y0'],
            ['coerce', 'Y0', 'Y1']
        ]
        self.assertEqual(expected, actual)

    def test_X_to_Y1(self):
        # X (string -> Y; Y -> X;) => Y1 (string -> Y; Y -> Y1)
        target = self._makeOne([
            ("string", "Y"), ("Y", "X"), ("Y", "Y1"),
        ])
        actual = target.resolve(src="X", dst="Y1")
        expected = [
            ['coerce', 'X', 'Y'],
            ['coerce', 'Y', 'Y1'],
        ]
        self.assertEqual(expected, actual)

    def test_X0_to_Y3(self):
        # X (string -> Y; Y -> X;) => Y1 (string -> Y; Y -> Y1)
        target = self._makeOne([
            ("string", "Y"), ("Y", "X"), ("Y", "Y1"),
        ])
        actual = target.resolve(src="X", dst="Y1")
        expected = [
            ['coerce', 'X', 'Y'],
            ['coerce', 'Y', 'Y1'],
        ]
        self.assertEqual(expected, actual)

