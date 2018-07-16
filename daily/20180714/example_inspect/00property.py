class reify(object):
    """cached property"""

    def __init__(self, wrapped):
        self.wrapped = wrapped
        try:
            self.__doc__ = wrapped.__doc__
        except:
            pass

    def __get__(self, inst, objtype=None):
        if inst is None:
            return self
        val = self.wrapped(inst)
        setattr(inst, self.wrapped.__name__, val)
        return val


class F:
    @property
    def x(self):
        return "x"

    @reify
    def y(self):
        return "y"


from pyinspect.inspect import inspect, Options

inspect(F, options=Options())
