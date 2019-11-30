from __future__ import annotations
import typing as t
import dataclasses


@dataclasses.dataclass(frozen=True)
class Act:
    name: str
    args: t.List[t.Any] = dataclasses.field(default_factory=list)
    kwargs: t.Dict[str, t.Any] = dataclasses.field(default_factory=dict)
    c: int = 0  # logical time
    id: int = 0
    parent_id: int = 0
    called: bool = False


@dataclasses.dataclass(frozen=False)
class Line:
    lineno: int
    args: t.List[Act] = dataclasses.field(default_factory=list)
    is_assign: bool = False


class CapturedMock:
    # todo: avoid name confict
    history: t.List[Act]
    children: t.Dict[str, CapturedMock]
    _c = 0

    @property
    def c(self):
        self.__class__._c += 1
        return self.__class__._c

    def __init__(self, *args, **kwargs):
        self.history = [
            Act(name="__init__", args=args, kwargs=kwargs, c=self.c, id=id(self))
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
            child._c = self._c

        self.history.append(
            Act(
                name=name,
                args=[],
                kwargs={},
                c=self.c,
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
                c=self.c,
                id=id(self),
                parent_id=id(self.parent),
                called=True,
            )
        )
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

                    past_line.args = left  # xxx: side-effect
                    past_line.is_assign = True

                    for i, x in enumerate(lines):
                        if id(x) == id(past_line):
                            break
                    right_line = dataclasses.replace(
                        past_line, args=right, is_assign=False
                    )
                    lines.insert(i + 1, right_line)
                    line.args.append((right[0]))
                    for x in right_line.args:
                        seen[x.id] = right_line

        if status == not_changed and act.name == "__call__":
            prev = line.args.pop()
            modified = dataclasses.replace(prev, args=act.args, kwargs=act.kwargs, called=True)
            line.args.append(modified)
        else:
            line.args.append(act)
        if status == created:
            seen[act.id] = line
        current_id = act.id

    if line.args:
        lines.append(line)
    return lines


def compile(lines: t.List[Line], *, root_name: str = "ROOT"):
    for line in lines:
        nodes = []
        args = line.args
        if line.is_assign:
            nodes.append(f"g{args[-1].id}")
            nodes.append(" = ")

        if args[0].name == "__init__":
            nodes.append(root_name)
            if args[0].called:
                nodes.append("(")
            if args[0].args:
                nodes.append(str(args[0].args))  # xxx
            if args[0].kwargs:
                nodes.append(str(args[0].kwargs))  # xxx
            if args[0].called:
                nodes.append(")")
        else:
            nodes.append(f"g{args[0].id}")
        for x in args[1:]:
            nodes.append(".")
            nodes.append(x.name)
            if x.called:
                nodes.append("(")
            if x.args:
                nodes.append(str(x.args))  # xxx
            if x.kwargs:
                nodes.append(str(x.kwargs))  # xxx
            if x.called:
                nodes.append(")")
        yield "".join(nodes)
