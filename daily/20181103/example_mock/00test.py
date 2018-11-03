# from: http://nao-y.hatenablog.com/entry/2018/08/01/231336

# note: not good

import unittest

from importlib import import_module, reload
from unittest.mock import patch


class FortuneTestCase(unittest.TestCase):
    @patch('random.choice', lambda x: 1)
    def test_bad(self):
        module = import_module('fortune')
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
