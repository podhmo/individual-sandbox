import typing as t
import dataclasses


class Base:
    def __init_subclass__(cls):
        hints = t.get_type_hints(cls)
        #         code = f"""\
        # def __init__(self, {args}) -> None:
        #     {assign}
        # """
        code = """\
def __init__(self, *, name:str) -> None:
    self.name = name
"""
        d = {}
        exec(code, d)
        cls.__init__ = d["__init__"]


class A(Base):
    name: str


print(A(name="foo"))
print(A(name="foo").name)
