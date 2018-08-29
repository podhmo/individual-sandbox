class Object:
    def __init__(self, name, attrs, *, tracer):
        self._name = name
        self._attrs = attrs
        self._tracer = tracer

    def __getattr__(self, name):
        attr = self._attrs.get(name)
        if attr is None:
            attr = self._attrs[name] = {}
        self._tracer.trace_access(name)
        return Object(f"{self._name}.{name}", attr, tracer=self._tracer)

    def __iter__(self):
        return iter([self.__getattr__('__iter__')()])

    def __call__(self, *args, **kwargs):
        self._tracer.trace_args(args, kwargs)
        return self

    def __repr__(self):
        return f"<{self.__class__.__name__} {self._name!r}>"

    def __add__(self, *args, **kwargs):
        return self.__getattr__('__add__')(*args, **kwargs)

    def __radd__(self, *args, **kwargs):
        return self.__getattr__('__radd__')(*args, **kwargs)

    def __mul__(self, *args, **kwargs):
        return self.__getattr__('__mul__')(*args, **kwargs)

    def __rmul__(self, *args, **kwargs):
        return self.__getattr__('__rmul__')(*args, **kwargs)

    def __div__(self, *args, **kwargs):
        return self.__getattr__('__div__')(*args, **kwargs)

    def __rdiv__(self, *args, **kwargs):
        return self.__getattr__('__rdiv__')(*args, **kwargs)

    def __truediv__(self, *args, **kwargs):
        return self.__getattr__('__truediv__')(*args, **kwargs)

    def __rtruediv__(self, *args, **kwargs):
        return self.__getattr__('__rtruediv__')(*args, **kwargs)

    def __floordiv__(self, *args, **kwargs):
        return self.__getattr__('__floordiv__')(*args, **kwargs)

    def __rfloordiv__(self, *args, **kwargs):
        return self.__getattr__('__rfloordiv__')(*args, **kwargs)

    def __mod__(self, *args, **kwargs):
        return self.__getattr__('__mod__')(*args, **kwargs)

    def __rmod__(self, *args, **kwargs):
        return self.__getattr__('__rmod__')(*args, **kwargs)

    def __pos__(self, *args, **kwargs):
        return self.__getattr__('__pos__')(*args, **kwargs)

    def __neg__(self, *args, **kwargs):
        return self.__getattr__('__neg__')(*args, **kwargs)

    def __getitem__(self, *args, **kwargs):
        return self.__getattr__('__getitem__')(*args, **kwargs)

    def __lt__(self, *args, **kwargs):
        return self.__getattr__('__lt__')(*args, **kwargs)

    def __le__(self, *args, **kwargs):
        return self.__getattr__('__le__')(*args, **kwargs)

    def __gt__(self, *args, **kwargs):
        return self.__getattr__('__gt__')(*args, **kwargs)

    def __ge__(self, *args, **kwargs):
        return self.__getattr__('__ge__')(*args, **kwargs)

    def __int__(self, *args, **kwargs):
        return self.__getattr__('__int__')(*args, **kwargs)

    def __float__(self, *args, **kwargs):
        return self.__getattr__('__float__')(*args, **kwargs)

    def __complex__(self, *args, **kwargs):
        return self.__getattr__('__complex__')(*args, **kwargs)

    def __pow__(self, *args, **kwargs):
        return self.__getattr__('__pow__')(*args, **kwargs)

    def __rpow__(self, *args, **kwargs):
        return self.__getattr__('__rpow__')(*args, **kwargs)

    def __sub__(self, *args, **kwargs):
        return self.__getattr__('__sub__')(*args, **kwargs)

    def __rsub__(self, *args, **kwargs):
        return self.__getattr__('__rsub__')(*args, **kwargs)


class Env:
    def __init__(self, *, tracer):
        self.env = {}
        self._tracer = tracer

    def __iter__(self):
        return iter([])  # xxx

    def __getitem__(self, name):
        subenv = self.env.get(name)
        if subenv is None:
            subenv = self.env[name] = {}
        ob = Object(name, subenv, tracer=self._tracer.make(name))
        return ob


class Tracer:
    def __init__(self, path):
        self.path = path

    def make(self, name):
        path = [{"access": name}]
        self.path.append(path)
        return self.__class__(path)

    def trace_access(self, name):
        frame = {"access": name}
        self.path.append(frame)
        return frame

    def trace_args(self, args, kwargs):
        self.path[-1]["args"] = args
        self.path[-1]["kwargs"] = kwargs
        return self.path[-1]

    def __repr__(self):
        return f"<{self.__class__.__name__} {self.path!r}>"
