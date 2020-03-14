import operator
from prestring.utils import LazyArgumentsAndKeywords


class Q:
    __slots__ = ("builder", "val", "kwargs")

    def __init__(self, builder, val, kwargs):
        self.builder = builder
        self.val = val
        self.kwargs = kwargs

    def __call__(self, *args, **kwargs):
        return self.builder.call(self, args, kwargs)

    # name from ast modules's node class name

    def And(self, right):
        return self.builder.bop(self, "and", right)

    def Or(self, right):
        return self.builder.bop(self, "or", right)

    def Gt(self, right):
        return self.builder.bop(self, ">", right)

    def GtE(self, right):
        return self.builder.bop(self, ">=", right)

    def Lt(self, right):
        return self.builder.bop(self, "<", right)

    def LtE(self, right):
        return self.builder.bop(self, "<=", right)

    def Eq(self, right):
        return self.builder.bop(self, "==", right)

    def NotEq(self, right):
        return self.builder.bop(self, "!=", right)

    def Not(self):
        return self.builder.uop(self, "not")

    def Is(self, right):
        return self.builder.bop(self, "is", right)

    def IsNot(self, right):
        return self.builder.bop(self, "is not", right)

    def In(self, right):
        return self.builder.bop(self, "in", right)

    def NotIn(self, right):
        return self.builder.bop(self, "not in", right)

    #
    def Add(self, right):
        return self.builder.bop(self, "+", right)

    def Sub(self, right):
        return self.builder.bop(self, "-", right)

    def Mult(self, right):
        return self.builder.bop(self, "*", right)

    def Div(self, right):
        return self.builder.bop(self, "/", right)

    def neg(self):
        return self.builder.uop(self, "-")

    #
    def __str__(self):
        return str(self.builder.build(self))

    def __getattr__(self, name):
        return self.builder.attr(self, name)


class QAttr:
    __slots__ = ("builder", "name", "_q")

    def __init__(self, builder, name, q):
        self.builder = builder
        self.name = name
        self._q = q

    def __call__(self, *args, **kwargs):
        new_args = [(x if hasattr(x, "builder") else repr(x)) for x in args]
        new_kwargs = {
            k: (x if hasattr(x, "builder") else repr(x)) for k, x in kwargs.items()
        }
        return self._q.__class__(
            self.builder,
            "{inner}.{methodname}({args})",
            kwargs=dict(
                inner=self._q,
                methodname=self.name,
                args=LazyArgumentsAndKeywords(new_args, new_kwargs),  # xxx
            ),
        )

    def __getattr__(self, name):
        if name.startswith("_"):
            raise AttributeError(name)
        return self.__class__(self.builder, f"{self.name}.{name}", self._q)


class QBuilder:
    ATTR_CLASS = QAttr

    def uop(self, q, name):
        fmt = "({op} {value})"
        return q.__class__(self, fmt, kwargs=dict(op=name, value=q))

    def bop(self, q, name, right):
        fmt = "({left} {op} {right})"
        return q.__class__(self, fmt, kwargs=dict(op=name, left=q, right=right))

    def attr(self, q, name):
        return self.ATTR_CLASS(self, name, q)

    def call(self, q, args, kwargs):
        new_args = [(x if hasattr(x, "builder") else repr(x)) for x in args]
        new_kwargs = {
            k: (x if hasattr(x, "builder") else repr(x)) for k, x in kwargs.items()
        }
        return q.__class__(
            self,
            "{inner}({args})",
            kwargs=dict(
                inner=q, args=LazyArgumentsAndKeywords(new_args, new_kwargs),  # xxx
            ),
        )

    def build(self, q):
        if not q.kwargs:
            return q.val

        kwargs = {
            k: (v.builder.build(v) if hasattr(v, "builder") else v)
            for k, v in q.kwargs.items()
        }
        return q.val.format(**kwargs)


class QEvalAttr:
    __slots__ = ("builder", "name", "_q")

    def __init__(self, builder, name, q):
        self.builder = builder
        self.name = name
        self._q = q

    def __call__(self, *args, **kwargs):
        new_args = [getattr(x, "val", x) for x in args]
        new_kwargs = {k: getattr(x, "val", x) for k, x in kwargs.items()}
        ob = getattr(self._q, "val", self._q)
        return getattr(ob, self.name)(*new_args, **new_kwargs)

    def __getattr__(self, name):
        if name.startswith("_"):
            raise AttributeError(name)
        return self.__class__(self.builder, f"{self.name}.{name}", self._q)


class QEvaluator:
    ATTR_CLASS = QEvalAttr

    uop_mapping = {
        "-": operator.neg,
        "not": operator.not_,
    }
    bop_mapping = {
        "and": lambda x, y: x and y,
        "or": lambda x, y: x or y,
        ">": operator.gt,
        ">=": operator.ge,
        "<": operator.lt,
        "<=": operator.le,
        "==": operator.eq,
        "!=": operator.ne,
        "is": operator.is_,
        "is not": operator.is_not,
        "in": operator.contains,
        "not in": lambda x, y: x not in y,
        "+": operator.add,
        "-": operator.sub,
        "*": operator.mul,
        "/": operator.truediv,
    }

    def uop(self, q: Q, name):
        op = self.uop_mapping[name]
        val = op(getattr(q, "val", q))
        return q.__class__(self, val, kwargs=None)

    def bop(self, q, name, right):
        op = self.bop_mapping[name]
        left_val = getattr(q, "val", q)
        right_val = getattr(right, "val", right)
        val = op(left_val, right_val)
        return q.__class__(self, val, kwargs=None)

    def attr(self, q, name):
        return self.ATTR_CLASS(self, name, q)

    def build(self, q):
        return q.val


def q(fmt, builder=QBuilder(), **kwargs):
    return Q(builder, fmt, kwargs=kwargs)
