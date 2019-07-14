from something import do_something

class A:
    """
    000
    001
    002
    003
    """

    def f(self, x):
        if x is None:
            return "[]"
        else:
            return f"[{x}]"

    def g(self, x):
        if x is None:
            do_something()
            do_something()
            do_something()

            def closure0(y):
                return f"[{y}]"  # <- ここを指定(18行目)

            return closure0
        else:
            do_something()
            do_something()
            do_something()

            # xxx:
            def closure1(y):  # zzzx
                return f"[{x}, {y}]"

            return closure1

    def h(self, x):
        if x is None:
            return "[]"
        else:
            return f"[{x}]"


class B:
    def f(self, x):
        if x is None:
            return "[]"
        else:
            return f"[{x}]"
