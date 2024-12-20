from dictknife.langhelpers import reify


class WrappedExceptionFactory:
    def __init__(self, *, prefix="Wrapped", mixin_class=None):
        self.prefix = prefix
        self.mixin_class = mixin_class or WrappedMixin
        self.classes = {}

    def __call__(self, e: Exception, *, where: str) -> Exception:
        if isinstance(e, self.mixin_class):
            e.stack.append(where)
            return e

        cls = self.classes.get(e.__class__)
        if cls is None:
            cls = type(
                f"Wrapped{e.__class__.__name__}", (WrappedMixin, e.__class__), {}
            )
            self.classes[e.__class__] = cls

        # sync
        exc = cls.__new__(cls).with_traceback(e.__traceback__)
        exc.__dict__.update(e.__dict__)
        if hasattr(e, "args"):
            exc.args = e.args
        return self(exc, where=where)


class WrappedMixin:
    @reify
    def stack(self):
        return []

    def __str__(self):
        return f"{super().__str__()} (where={self.stack})"


f = WrappedExceptionFactory()
try:
    raise KeyError("hmm")
except KeyError as e:
    e2 = f(e, where="<toplevel>")
    print(e, "@", e2)
