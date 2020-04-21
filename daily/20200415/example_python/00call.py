import inspect
import typed_ast.ast3 as ast
from typing import Callable, Any


def f(x):
    y = x + 1
    return x + y


def use(fn: Callable[..., Any]):
    t = ast.parse(inspect.getsource(fn))
    assert len(t.body) == 1
    body = t.body[0].body

    for node in body:
        print(ast.dump(node))


use(f)
# Assign(targets=[Name(id='y', ctx=Store())], value=BinOp(left=Name(id='x', ctx=Load()), op=Add(), right=Num(n=1)), type_comment=None)
# Return(value=BinOp(left=Name(id='x', ctx=Load()), op=Add(), right=Name(id='y', ctx=Load())))
