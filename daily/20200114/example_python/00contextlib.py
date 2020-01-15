class _GeneratorContextManager:
    def __init__(self, func, *args, **kwargs):
        self.gen = func(*args, **kwargs)

    def __enter__(self):
        return next(self.gen)

    def __exit__(self, type, value, traceback):
        print(f"@ {type=} {value=} {traceback=}")
        if type is None:
            try:
                next(self.gen)
            except StopIteration:
                return False
            else:
                raise RuntimeError("generator didn't stop")
        else:
            if value is None:
                # Need to force instantiation so we can reliably
                # tell if we get the same exception back
                value = type()
            return True


class Fail(Exception):
    pass


def if_(cond):
    print(cond)
    if not cond:
        raise Fail()
    yield cond


with _GeneratorContextManager(if_, True) as cond:
    print("ok", cond)
with _GeneratorContextManager(if_, False) as cond:
    print("ng", cond)
