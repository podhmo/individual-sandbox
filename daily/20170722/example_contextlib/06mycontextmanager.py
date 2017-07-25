import sys
from functools import wraps


class ContextDecorator(object):
    def _recreate_cm(self):
        return self

    def __call__(self, func):
        @wraps(func)
        def inner(*args, **kwds):
            with self._recreate_cm():
                return func(*args, **kwds)

        return inner


class _GeneratorContextManager(ContextDecorator):
    def __init__(self, func, args, kwds):
        self.gen = func(*args, **kwds)
        self.func, self.args, self.kwds = func, args, kwds
        # Issue 19330: ensure context manager instances have good docstrings
        doc = getattr(func, "__doc__", None)
        if doc is None:
            doc = type(self).__doc__
        self.__doc__ = doc
        # Unfortunately, this still doesn't provide good help output when
        # inspecting the created context manager instances, since pydoc
        # currently bypasses the instance docstring and shows the docstring
        # for the class instead.
        # See http://bugs.python.org/issue19404 for more details.

    def _recreate_cm(self):
        # _GCM instances are one-shot context managers, so the
        # CM must be recreated each time a decorated function is
        # called
        return self.__class__(self.func, self.args, self.kwds)

    def __enter__(self):
        try:
            return next(self.gen)
        except StopIteration:
            raise RuntimeError("generator didn't yield") from None

    def __exit__(self, type, value, traceback):
        if type is None:
            try:
                next(self.gen)
            except StopIteration:
                return
            else:
                raise RuntimeError("generator didn't stop")
        else:
            print("@", type, value, traceback)
            if value is None:
                print("@1", type, value, traceback)
                # Need to force instantiation so we can reliably
                # tell if we get the same exception back
                value = type()
            try:
                print("@2", type, value, traceback)
                self.gen.throw(type, value, traceback)
                raise RuntimeError("generator didn't stop after throw()")
            except StopIteration as exc:
                print("@3", type, value, traceback)
                # Suppress StopIteration *unless* it's the same exception that
                # was passed to throw().  This prevents a StopIteration
                # raised inside the "with" statement from being suppressed.
                print("#", exc is not value)
                return exc is not value
            except RuntimeError as exc:
                print("@4", type, value, traceback)
                # Likewise, avoid suppressing if a StopIteration exception
                # was passed to throw() and later wrapped into a RuntimeError
                # (see PEP 479).
                if exc.__cause__ is value:
                    return False
                raise
            except:
                print("@5", type, value, traceback)
                # only re-raise if it's *not* the exception that was
                # passed to throw(), because __exit__() must not raise
                # an exception unless __exit__() itself failed.  But throw()
                # has to raise the exception to signal propagation, so this
                # fixes the impedance mismatch between the throw() protocol
                # and the __exit__() protocol.
                #
                if sys.exc_info()[1] is not value:
                    raise


def contextmanager(func):
    @wraps(func)
    def helper(*args, **kwds):
        return _GeneratorContextManager(func, args, kwds)

    return helper


@contextmanager
def ob():
    ob = object()
    print("")
    print("<<< before", id(ob))
    yield ob
    print(">>> after", id(ob))


def test_it(ob):
    print("**test", id(ob), "**")
    1 / 0


try:
    with ob() as x:
        test_it(x)
except Exception as e:
    print(e)
