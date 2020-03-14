import typing as t
import operator


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

    def __to_string__(self, builder) -> str:
        if not self.kwargs:
            return self.val
        kwargs = {
            k: (v.__to_string__(builder) if hasattr(v, "__to_string__") else v)
            for k, v in self.kwargs.items()
        }
        return self.val.format(**kwargs)

    def __getattr__(self, name):
        if name.startswith("_"):
            raise AttributeError(name)
        return self.builder.getattr(self, name)

    def __getitem__(self, name):
        return self.builder.getindex(self, name)


class QArgs:
    __slots__ = ("builder", "args", "kwargs")

    def __init__(self, builder, args, kwargs):
        self.builder = builder
        self.args = args
        self.kwargs = kwargs

    def __to_string__(self, builder) -> str:
        new_args = [
            (x.__to_string__(builder) if hasattr(x, "__to_string__") else repr(x))
            for x in self.args
        ]
        new_kwargs = [
            "{}={}".format(
                k,
                (x.__to_string__(builder) if hasattr(x, "__to_string__") else repr(x)),
            )
            for k, x in self.kwargs.items()
        ]
        r = []
        if new_args:
            r.extend(new_args)
        if new_kwargs:
            r.extend(new_kwargs)
        return ", ".join(r)


class QBuilder:
    uop_mapping: t.ClassVar[t.Dict[str, t.Union[str, t.Callable[..., t.Any]]]] = {}
    bop_mapping: t.ClassVar[t.Dict[str, t.Union[str, t.Callable[..., t.Any]]]] = {}

    def uop(self, q, name):
        fmt = "({op} {value})"
        name = self.uop_mapping.get(name, name)
        if callable(name):
            return name(self, q, name)
        return q.__class__(self, fmt, kwargs=dict(op=name, value=q))

    def bop(self, q, name, right):
        fmt = "({left} {op} {right})"
        name = self.bop_mapping.get(name, name)
        if callable(name):
            return name(self, q, name, right)
        return q.__class__(self, fmt, kwargs=dict(op=name, left=q, right=right))

    def getattr(self, q, name):
        fmt = "{inner}.{name}"
        return q.__class__(self, fmt, kwargs=dict(inner=q, name=name))

    def getindex(self, q, name):
        return q.__class__(self, "{inner}[{name}]", kwargs=dict(inner=q, name=name))

    def call(self, q, args, kwargs):
        return q.__class__(
            self,
            "{inner}({args})",
            kwargs=dict(inner=q, args=QArgs(self, args, kwargs)),
        )

    def build(self, q):
        return q.__to_string__(self)


class QJSBuilder(QBuilder):
    uop_mapping = {
        "not": "!",
    }
    bop_mapping = {
        "and": "&&",
        "or": "||",
        "is": "===",
        "is not": "!==",
        # "in": operator.contains,
        # "not in": lambda x, y: x not in y,
    }


class QEvaluator:
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

    def getattr(self, q, name):
        ob = getattr(q, "val", q)
        return q.__class__(self, getattr(ob, name), kwargs=None)

    def getindex(self, q, name):
        ob = getattr(q, "val", q)
        return q.__class__(self, ob[getattr(name, "val", name)], kwargs=None)

    def call(self, q, args, kwargs):
        args = [getattr(x, "val", x) for x in args]
        kwargs = {k: getattr(x, "val", x) for k, x in kwargs.items()}
        return q.__class__(self, q.val(*args, **kwargs), kwargs=None,)

    def build(self, q):
        return q.val


def q(fmt, builder=QBuilder(), **kwargs):
    return Q(builder, fmt, kwargs=kwargs)
