import typing as t


@decorator("after")
def foo(x, y, z) -> t.Optional[int]:
    """after"""
    pass


def boo(x) -> int:
    """after"""
    pass


class A:
    """after"""

    def f(self, x):  # oyoyo
        """after"""
        pass

    def g(self, *args, **kwargs):  # oyoyo
        """after"""
        pass

    def h(self, x):
        """after"""
        pass


if x == "hmm":

    def hidden():
        pass
