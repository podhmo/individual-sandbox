from dataclasses import dataclass
import inspect
import ast
import traceback
import sys
import linecache


def transform(fn, *, globals: dict):
    """
    transform

    ```
    <code>
    ```

    to

    ```
    # try:
        _exception = None
        <code>
    except Exception as exc:
        _exception = exc
    finally:
        return locals()
    ```
    """
    t: ast.Module = ast.parse(inspect.getsource(fn))
    assert len(t.body) == 1, len(t.Body)
    node = t.body[0]
    assert isinstance(node, ast.FunctionDef), type(node)

    node_body = node.body
    node.body = [
        ast.Assign(
            targets=[ast.Name(id="_exception", ctx=ast.Store())],
            value=ast.Constant(value=None),
        ),
        ast.Try(
            body=node_body,
            handlers=[
                ast.ExceptHandler(
                    type=ast.Name(id="Exception", ctx=ast.Load()),
                    name="exc",
                    body=[
                        ast.Assign(
                            targets=[ast.Name(id="_exception", ctx=ast.Store())],
                            value=ast.Name(id="exc", ctx=ast.Load()),
                        )
                    ],
                )
            ],
            orelse=[],
            finalbody=[
                ast.Return(
                    value=ast.Call(
                        func=ast.Name(id="locals", ctx=ast.Load()), args=[], keywords=[]
                    )
                )
            ],
        ),
    ]

    ast.fix_missing_locations(node)
    offset = fn.__code__.co_firstlineno - 1
    for x in ast.walk(node):
        if hasattr(x, "lineno"):
            x.lineno += offset
    
    # todo: bind fn.__code__ (side effect)
    filename = fn.__code__.co_filename
    # filename = "<ast>"
    code = compile(t, filename, "exec")
    D = {}
    exec(code, globals, D)
    transformed = D[fn.__name__]
    # transformed.__code__.co_firstlineno = fn.__code__.co_firstlineno
    return transformed


@dataclass
class Point:
    x: int
    y: int


@dataclass
class Pair:
    left: Point
    right: Point


def run_dsl(run, *, globals=None, depth=1):
    if globals is None:
        frame = sys._getframe(depth)
        globals = frame.f_globals

    fn = transform(run, globals=globals)

    resources = fn(y=20)
    exc :Exception = resources.pop("_exception", None)
    if exc is not None:
        # te = traceback.TracebackException.from_exception(exc)
        # print("".join(te.format()))
        raise exc from None

    return resources


def run(y: int) -> dict:
    p0 = Point(x=10, y=y)
    f(y)
    p1 = Point(x=20, y=y)


def f(y):
    p0 = Point(x=10, y=y)
    p1 = Point(x=20, y=y0)

run_dsl(run)
