@decorator("before")
def foo(x, y) -> int:
    """before"""
    pass


def bar(x) -> int:
    """before"""
    pass


class A:
    """before"""

    def f(self):
        """before"""
        pass

    # hmm
    def g(self):  # hoho
        pass
