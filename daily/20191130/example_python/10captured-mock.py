from __future__ import annotations
import typing as t
import dataclasses


@dataclasses.dataclass(frozen=True)
class Act:
    name: str
    args: t.List[t.Any] = dataclasses.field(default_factory=list)
    kwargs: t.Dict[str, t.Any] = dataclasses.field(default_factory=dict)
    time: int = 0  # logical time
    id: int = 0
    parent_id: int = 0


@dataclasses.dataclass(frozen=False)
class Line:
    lineno: int
    args: t.List[Act] = dataclasses.field(default_factory=list)


class CapturedMock:
    # todo: avoid name confict
    history: t.List[Act]
    children: t.Dict[str, CapturedMock]
    _c = 0

    @property
    def c(self):
        self._c += 1
        return self._c

    def __init__(self, *args, **kwargs):
        self.history = [
            Act(name="__init__", args=args, kwargs=kwargs, time=self.c, id=id(self))
        ]
        self.children = {}
        self.parent = None

    def __getattr__(self, name):
        child = self.children.get(name)
        if child is None:
            child = self.children[name] = self.__class__.__new__(self.__class__)
            child.history = self.history
            child.children = {}
            child.parent = self
        self.history.append(
            Act(
                name=name,
                args=[],
                kwargs={},
                time=self.c,
                id=id(child),
                parent_id=id(child.parent),
            )
        )
        return child

    def __call__(self, *args, **kwargs):
        self.history.append(
            Act(
                "__call__",
                args=args,
                kwargs=kwargs,
                time=self.c,
                id=id(self),
                parent_id=id(self.parent),
            )
        )
        # called?
        return self


# [sentence] :: [assign]  | [no-assign]
# [assign] :: <name> '=' <name> { [access] | [call] }*
# [no-assign] :: <name> { [access] | [call] }*
# [access] :: '.' <name>
# [method call] :: '.' <name> '(' ')'


def scan(x: CapturedMock):
    """aggregated as oneline-action"""

    seen: t.Dict[int, Line] = {}  # act.id -> line
    current_id = 0
    created, changed, not_changed = "created", "changed", "not_changed"
    status = changed
    is_same_sentence = True

    lines = []
    i = 0
    line = Line(lineno=i)

    for act in x.history:
        if current_id == act.id:
            status = not_changed
        elif act.id in seen:
            status = changed
        else:
            status = created

        is_same_sentence = current_id == act.parent_id or current_id == act.id
        if not is_same_sentence:
            lines.append(line)
            i += 1
            line = Line(lineno=i)

            if act.parent_id in seen:
                past_line = seen[act.parent_id]
                if past_line.args[0].id == act.parent_id:
                    line.args.append((past_line.args[0]))
                else:
                    # separation
                    for i, prev in enumerate(past_line.args):
                        if prev.id == act.parent_id:
                            break
                    left, right = past_line.args[: i + 1], past_line.args[i:]
                    assert left[-1].id == act.parent_id
                    assert right[0].id == act.parent_id
                    past_line.args = left
                    for i, x in enumerate(lines):
                        if id(x) == id(past_line):
                            break
                    lines.insert(i + 1, dataclasses.replace(past_line, args=right))
                    line.args.append((right[0]))

        if status == not_changed:
            assert act.name == "__call__"
            prev = line.args.pop()
            modified = dataclasses.replace(prev, args=act.args, kwargs=act.kwargs)
            line.args.append(modified)
        else:
            line.args.append(act)
        if status == created:
            seen[act.id] = line
        current_id = act.id

    if line.args:
        lines.append(line)
    return lines


def trace(x: CapturedMock, *, name="ROOT"):
    for act in x.history:
        if act.name == "__init__":
            print(f"g{act.id} = {name}({act.args}, {act.kwargs})")
        else:
            print(f"g{act.id}.{act.name}")


x = CapturedMock()
x.y.z(10)

for line in scan(x):
    print(line)

print("-")

x = CapturedMock()
x.y
x.z(10)

for line in scan(x):
    print(line)

print("-")
x = CapturedMock()
y = x.y
y.z0.m(20)
y.z1.m(30)

for line in scan(x):
    print(line)
