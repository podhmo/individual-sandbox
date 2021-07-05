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
    # todo: bind fn.__code__ (side effect)
    filename = fn.__code__.co_filename
    # filename = "<ast>"
    code = compile(t, filename, "exec")
    D = {}
    exec(code, globals, D)
    return D[fn.__name__]


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
    run1(y)
    p1 = Point(x=20, y=y0)


def run1(y):
    p0 = Point(x=10, y=y)
    p1 = Point(x=20, y=y0)


fn = transform(run, globals=globals())
D = fn(y=20)
if D["_exception"] is not None:
    exc :Exception = D["_exception"]
    te = traceback.TracebackException.from_exception(exc)

    print("@Traceback (most recent call last)")
    for frame in te.stack:
        if frame.name == run.__name__ and frame.filename == run.__code__.co_filename:  # xxx
            offset = run.__code__.co_firstlineno -1
            print(f"@  File '{frame.filename}', line {frame.lineno + offset}, in {frame.name}")
            print(f"@    {linecache.getline(frame.filename, frame.lineno + offset).strip()}")
        else:
            print(f"*  File '{frame.filename}', line {frame.lineno}, in {frame.name}")
            print(f"*    {frame.line}")

    #  print("".join(te.format()))
print(D)
