import contextlib


class Fail(Exception):
    pass


class if_:
    def __init__(self, cond):
        self.cond = cond

    def __enter__(self):
        return self.cond

    def __exit__(self, typ, val, tb):
        if typ is not None:
            if val is None:
                val = typ()
            assert isinstance(val, Fail)
            assert self.cond is False, f"must be {self.cond}"
        return True


with if_(True) as x:
    raise Fail()
    print("ok", x)
with if_(False) as x:
    print("ng", x)
# with wrap(if_(False)):
#     print("ok")
