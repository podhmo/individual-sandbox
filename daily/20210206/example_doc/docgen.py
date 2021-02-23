from __future__ import annotations
import typing
from handofcats import as_command
from magicalimport import import_module
from metashape.runtime import get_walker
from metashape.marker import guess_mark
from metashape import typeinfo


@as_command
def run(name: str):
    m = import_module(name)
    w = get_walker(m, aggressive=True, recursive=False)

    def _to_str_typeinfo(info: typeinfo.TypeInfo) -> str:
        """
        >>> _to_str_typeinfo(str)
        'str'

        >>> _to_str_typeinfo(Literal["x", "y", "z"])
        ' `x, y, z` '

        >>> Color = Literal["Red", "Green", "Blue"]
        >>> _to_str_typeinfo(Color)
        '[Color](#Color)'
        """
        if info.underlying != info.type_:  # enum
            name = w.resolver.resolve_typename(info.type_)
            if name != "_GenericAlias":
                return f"[{name}](#{name})"
            args = [str(x) for x in typing.get_args(info.type_)]
            return f" `{', '.join(args)}` "

        name = info.underlying.__name__
        if not info.user_defined_type:
            return name
        return f"[{name}](#{name})"

    for cls in w.walk(kinds=["object", "enum"]):
        print("")
        print(f"## {cls.__name__}")

        kind = guess_mark(cls)  # todo: use resolve
        if kind == "enum":
            print("")
            print("```")
            args = typing.get_args(cls)
            print(f"   {args[0]}")
            for x in args[1:]:
                print(f" | {x}")
            print("```")
            print("")
        else:
            doc = w.resolver.metadata.resolve_doc(cls, verbose=True)
            if doc is not None:
                print("")
                print(doc)

            print("")
            print("| name | type | description |")
            print("| :--- | :--- | :--- |")
            for name, info, metadata in w.walk_fields(cls):
                print(
                    f"| {name} | {typeinfo.to_string(info, to_str=_to_str_typeinfo)} | {metadata.get('description') or ''}|"
                )
