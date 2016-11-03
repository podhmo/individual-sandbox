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

    # with type cast
    def test_px_to_y(self):
        # PX (string* -> PX) => Y (string -> Y)
        # (cast string*) -> deref -> (cast Y)
        target = self._makeOne([
            (["pointer", "string"], "PX"), ("string", "Y")
        ])
        actual = target.resolve(src="PX", dst="Y")
        expected = [
            ['coerce', 'PX', ('pointer', 'string')],
            ['coerce', ('pointer', 'string'), 'string'],
            ['coerce', 'string', 'Y']
        ]
        self.assertEqual(expected, actual)

    def test_y_to_px(self):
        # Y (string -> Y) => PX (string* -> PX)
        # (cast string) -> ref -> (cast PX)
        target = self._makeOne([
            (["pointer", "string"], "PX"), ("string", "Y")
        ])
        actual = target.resolve(src="Y", dst="PX")
        expected = [
            ['coerce', 'Y', 'string'],
            ['coerce', 'string', ('pointer', 'string')],
            ['coerce', ('pointer', 'string'), 'PX']
        ]
        self.assertEqual(expected, actual)

    def test_pointer_pointer_string_to_px(self):
        target = self._makeOne([
            (["pointer", "string"], "PX"), ("string", "Y")
        ])
        actual = target.resolve(src=("pointer", "pointer", "string"), dst="PX")
        expected = [
            ['coerce', ('pointer', 'pointer', 'string'), ('pointer', 'string')],
            ['coerce', ('pointer', 'string'), 'PX']
        ]
        self.assertEqual(expected, actual)

    def test_pointer_pointer_string_to_Y(self):
        target = self._makeOne([
            (["pointer", "string"], "PX"), ("string", "Y")
        ])
        actual = target.resolve(src=("pointer", "pointer", "pointer", "string"), dst="Y")
        expected = [
            ['coerce', ('pointer', 'pointer', 'pointer', 'string'), ('pointer', 'string')],
            ['coerce', ('pointer', 'string'), 'string'],
            ['coerce', 'string', 'Y']
        ]
        self.assertEqual(expected, actual)

    def test_string_to_pointer_pointer_string(self):
        target = self._makeOne([
        ])
        actual = target.resolve(src="string", dst=("pointer", "pointer", "pointer", "string"))
        expected = [
            ['coerce', 'string', ('pointer', 'pointer', 'pointer', 'string')]
        ]
        self.assertEqual(expected, actual)

    def test_x_to_pointer_pointer_string(self):
        target = self._makeOne([
            ("string", "X")
        ])
        actual = target.resolve(src="X", dst=("pointer", "pointer", "pointer", "string"))
        expected = [
            ['coerce', 'X', 'string'],
            ['coerce', 'string', ('pointer', 'pointer', 'pointer', 'string')]
        ]
        self.assertEqual(expected, actual)
