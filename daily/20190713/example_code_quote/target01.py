from something import do_something


class A:
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

            def closure1(y):
                return f"[{x}, {y}]"

            return closure1

    def h(self, x):
        if x is None:
            return "[]"
        else:
            return f"[{x}]"
