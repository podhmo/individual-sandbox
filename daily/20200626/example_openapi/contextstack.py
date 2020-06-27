from __future__ import annotations
import typing as t


C = t.TypeVar("C")
T = t.TypeVar("T")


class ContextStack(t.Generic[C]):
    stack: t.List[C]

    def __init__(self, *, factory: t.Callable[[t.List[t.Any]], C]) -> None:
        self.stack = []
        self.factory = factory

    @property
    def current(self) -> C:
        return self.stack[-1]

    def push(self, args: t.List[t.Any]) -> None:
        v = self.factory(args)
        self.stack.append(v)

    def pop(self) -> C:
        return self.stack.pop()


class ArgsAttr(t.Generic[T]):
    def __init__(self, names: t.List[str], *, factory: t.Callable[[str], T]) -> None:
        self._names = names
        self._factory = factory

    def __getattr__(self, name: str) -> T:
        if name not in self._names:
            raise AttributeError(name)

        # lazy initialization
        attr = self._factory(name)
        setattr(self, name, attr)
        return attr

    def __repr__(self) -> str:
        return f"ArgsAttr(args={self._names!r})"


if __name__ == "__main__":
    import unittest
    import dataclasses

    @dataclasses.dataclass(eq=False)
    class Arg:
        name: str
        summary: t.Optional[str] = None
        description: t.Optional[str] = None

    def create_default_runtime_context() -> ContextStack[ArgsAttr[Arg]]:
        def arg_factory(name: str) -> Arg:
            return Arg(name=name)

        def args_attr_factory(names: t.List[t.Any]) -> ArgsAttr[Arg]:
            return ArgsAttr(names, factory=arg_factory)

        s = ContextStack(factory=args_attr_factory)
        s.push([])  # root
        return s

    class Tests(unittest.TestCase):
        def _makeOne(self) -> ContextStack[ArgsAttr[Arg]]:
            return create_default_runtime_context()

        def test_current_instance(self) -> None:
            s = self._makeOne()
            self.assertEqual(s.current._names, [])  # root
            s.push(["x", "y"])
            self.assertIsInstance(s.current, ArgsAttr)
            self.assertEqual(s.current._names, ["x", "y"])
            s.pop()
            self.assertEqual(s.current._names, [])  # root

        def test_attr_ok(self) -> None:
            s = self._makeOne()
            s.push(["x", "y"])
            self.assertEqual(s.current.x.name, "x")

        def test_attr_ng(self) -> None:
            s = self._makeOne()
            s.push(["x", "y"])
            with self.assertRaises(AttributeError):
                s.current.z

    unittest.main()
