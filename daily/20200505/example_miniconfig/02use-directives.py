from handofcats import as_command
from miniconfig import Configurator as _Configurator


q = []


class Configurator(_Configurator):
    pass


def includeme(c: Configurator) -> None:
    def add(c: Configurator, task: str) -> None:
        def register():
            q.append(task)

        c.action(object(), register)

    c.add_directive("add", add)


def include_xxx(c: Configurator):
    for i in range(3):
        c.add(f"x{i}")


def include_yyy(c: Configurator):
    for i in range(3):
        c.add(f"y{i}")


def include_zzz(c: Configurator):
    for i in range(3):
        c.add(f"z{i}")


@as_command
def run():
    c = Configurator()

    c.include(includeme)
    c.include(include_xxx)
    c.include(include_zzz)

    print(f"before commit q={q}")
    c.commit()
    print(f"after commit q={q}")
