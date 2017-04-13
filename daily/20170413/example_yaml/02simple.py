import sys
import itertools as it
from dictknife import loading
from collections import OrderedDict

"""
hmm

- 単に置き換えるだけ -> 名前は
- dictをupdate -> squash dict
- listに対する変換 -> squash list?

次:
 引数も取りたい
 importできるようにしたい

import ast
ast.dump(ast.parse("foo(x, y)"))
"Module(body=[Expr(value=Call(func=Name(id='foo', ctx=Load()), args=[Name(id='x', ctx=Load()), Name(id='y', ctx=Load())], keywords=[]))])"
"""

loading.setup()
loads = loading.loads


def dumps(d):
    return loading.dumps(d)


s = """
me:
  $suffix:
    $suffix:
      person:
        name: foo
        age: 20
    suffix: +
  suffix: +
items:
  $ntimes:
    - x
    - y
  n: 3
"""


class Evaluator:
    def __init__(self, m):
        self.m = m

    def eval(self, d):
        if hasattr(d, "keys"):
            method = None
            kwargs = OrderedDict()
            for k in list(d.keys()):
                if k.startswith("$"):
                    if method is not None:
                        raise RuntimeError("conflicted: {!r} and {!r}".format(method, k))
                    method = k
                else:
                    v = self.eval(d[k])
                    kwargs[k] = v
            if method is None:
                return kwargs
            return self.apply(method[1:], self.eval(d[method]), kwargs=kwargs)
        elif isinstance(d, (list, tuple)):
            return [self.eval(x) for x in d]
        else:
            return d

    def apply(self, name, d, kwargs):
        method = getattr(self.m, name)
        return method(d, **kwargs)


def suffix(d, suffix=":"):
    return {k + suffix: v for k, v in d.items()}


def ntimes(d, n=2):
    return list(it.chain.from_iterable(it.repeat(d, n)))


evalator = Evaluator(sys.modules[__name__])
print(dumps(loads(s)))
print("----------------------------------------")
print(dumps(evalator.eval(loads(s))))
