import dis
import sys
import os
import typing as t
import logging
import inspect
from collections import ChainMap
from dataclasses import dataclass

logger = logging.getLogger(__name__)


class Point:
    x: int
    y: int


_registry: t.Dict[t.Type[t.Any], t.Type[t.Any]] = {}


def get_dataclass_factory(
    K: t.Type[t.Any],
    *,
    registry: t.Optional[t.Dict[t.Type[t.Any], t.Type[t.Any]]] = None,
) -> t.Type[t.Any]:
    global _registry
    if registry is None:
        registry = _registry
    factory = registry.get(K)
    if factory is not None:
        return factory
    factory = registry[K] = dataclass(K)
    return factory


def iter_instructions(
    fn: t.Callable[..., t.Any]
) -> t.Iterator[t.Tuple[int, dis.Instruction]]:
    lineno = -1
    for inst in dis.Bytecode(fn):
        if inst.starts_line is not None:
            lineno = inst.starts_line
        yield lineno, inst


def evaluate(fn: t.Callable[..., t.Any], *, _globals=None, **_locals: t.Any) -> None:
    sig = inspect.signature(fn)
    for k, p in sig.parameters.items():
        if k in _locals:
            continue
        if p.default == p.empty:
            continue
        _locals[k] = p.default
    inspect.getcallargs(fn, **_locals)

    # dis.dis(fn)

    if _globals is None:
        _globals = ChainMap(globals(), sys.modules["builtins"].__dict__)

    pos = []
    stack = []
    pos.append(stack)

    for lineno, inst in iter_instructions(fn):
        opname = inst.opname
        logger.debug("%s %s", inst.opname, inst.argrepr)
        if opname == "LOAD_GLOBAL":
            stack.append(_globals[inst.argval])
        elif opname == "LOAD_FAST":
            stack.append(_locals[inst.argval])
        elif opname == "LOAD_CONST":
            stack.append(inst.argval)
        elif opname == "STORE_FAST":
            # remove from stack? or not?
            _locals[inst.argval] = stack.pop()
        elif opname == "POP_TOP":
            stack.pop()
        elif opname == "RETURN_VALUE":
            # NG??
            assert len(stack) == 1, stack
            return stack[-1]
        elif opname == "BINARY_ADD":
            rv = stack.pop()
            lv = stack.pop()
            stack.append(lv + rv)
        elif opname == "BUILD_TUPLE":
            args = reversed([stack.pop() for _ in range(inst.arg)])
            stack.append(tuple(args))
        elif opname == "CALL_FUNCTION":
            args = reversed([stack.pop() for _ in range(inst.arg)])
            fn = stack.pop()
            stack.append(fn(*args))
        elif opname == "CALL_FUNCTION_KW":
            kwargs = {k: stack.pop() for k in stack.pop()}
            arglen = inst.arg - len(kwargs)
            args = ()
            if arglen > 0:
                args = reversed([stack.pop() for _ in range(inst.arg - len(kwargs))])
            fn = stack.pop()
            if fn.__module__ == __name__ and isinstance(fn, type):
                fn = get_dataclass_factory(fn)
            stack.append(fn(*args, **kwargs))
        else:
            # TODO: traceback
            raise RuntimeError(
                f"unsupported opcode: {inst.opname} {inst.argrepr}, in line {lineno}\n stack(len={len(stack)})={stack!r}"
            )


# def use(*, x:int=1) -> None:
#     pt = get_dataclass_factory(Point)(x=x,y=20)
#     print(pt)


def use(*, x: int) -> None:
    lpt = Point(x, y=20)
    print("lpt == ", lpt)

    rpt = Point(x+ 10, y=20)
    pair = (lpt, rpt)
    return pair


log_level = logging.DEBUG if bool(os.getenv("DEBUG")) else logging.INFO
logging.basicConfig(level=log_level)
# use(x=10)
print(evaluate(use, x=10))
