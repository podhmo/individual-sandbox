import unittest
import unittest.mock as mock


class Test(unittest.TestCase):
    def test_it(self):
        box = []

        def _use_input(text):
            box.append(text)

        with mock.patch("xxx.use_input") as m:
            m.side_effect = _use_input
            from xxx import do_something2

            do_something2()

        # assertion (このあたりは各自お好みの方法で)
        self.assertEqual(len(box), 1)
        self.assertEqual(box[0], "xxxx")


unittest.main()
