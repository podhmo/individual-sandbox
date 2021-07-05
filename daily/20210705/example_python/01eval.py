from dataclasses import dataclass
import inspect
import ast


@dataclass
class Point:
    x: int
    y: int


@dataclass
class Pair:
    left: Point
    right: Point


def run(y: int) -> dict:
    p0 = Point(x=10, y=y)
    p1 = Point(x=20, y=y)


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
    # todo: bind fn.__code__ (side effect)
    # filename = fn.__code__.co_filename + ".transform.py"
    code = compile(t, "<ast>", "exec")
    D = {}
    exec(code, globals, D)
    return D[fn.__name__]


# print(run(20))
fn = transform(run, globals=globals())
print(fn(y=20))
