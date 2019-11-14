@decorator("before")
def foo(x, y) -> int:
    """before"""

    def _internal(x, y, z):
        return x + y + z

    return _internal(x, y, y)


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
