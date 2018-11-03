import unittest

from unittest.mock import patch
import contextlib


def _fresh_import(module):
    from importlib import reload
    return reload(module)


class FortuneTestCase(unittest.TestCase):
    @patch('random.choice', lambda x: 1)
    def test_bad(self):
        from fortune import fortune
        expect = ('凶')
        self.assertEqual(module.fortune, expect)

    @patch('random.choice', lambda x: 3)
    def test_good(self):
        module = import_module('fortune')
        reload(module)
        expect = ('大吉')
        self.assertEqual(module.fortune, expect)


if __name__ == "__main__":
    unittest.main()
