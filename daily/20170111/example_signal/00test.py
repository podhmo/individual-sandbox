import sys
import unittest
import signal


class Ob(object):
    def __str__(self):
        return hex(id(self))


def do_something(calculate, _on_trap=None):
    ob = Ob()

    def on_trap(signum, frame):
        if _on_trap is not None:  # for test
            _on_trap(ob)
        print("cleanup with ", ob)
        sys.exit(1)

    signal.signal(signal.SIGHUP, on_trap)
    signal.signal(signal.SIGINT, on_trap)
    signal.signal(signal.SIGTERM, on_trap)

    # fetch anything?

    calculate(ob)  # do something

    # save db?


class Tests(unittest.TestCase):
    def test_it(self):
        from multiprocessing import Process, Queue
        import time

        q = Queue()
        init = 1
        called = 10
        q.put(init)

        def calculate(ob):
            print("before calculate", ob)
            time.sleep(1)  # waiting for killed
            print("after calculate", ob)

        def _on_trap(ob):
            self.assertEqual(q.get(), init)
            q.put(called)

        p = Process(target=lambda: do_something(calculate, _on_trap=_on_trap))
        p.start()
        time.sleep(0.1)
        p.terminate()  # SIGTERM
        p.join()
        self.assertEqual(q.get(), called)

if __name__ == "__main__":
    unittest.main()
    # before calculate 0x1030b66d8
    # cleanup with  0x1030b66d8
