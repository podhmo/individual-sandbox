from __future__ import annotations
import typing as t
import sys
from metashape.runtime import get_walker
from prestring.python.codeobject import Module
from prestring.utils import LazyArgumentsAndKeywords, LazyFormat


class Person:
    name: str
    age: t.Optional[int]
    # father: t.Optional[Person]
    father: Person


w = get_walker(sys.modules[__name__], aggressive=True)
m = Module()
Schema = m.from_("marshmallow").import_("Schema")
fields = m.from_("marshmallow").import_("fields")
schema_classes = {}


def dispatch_type(fields, info):
    typ = info.type_
    if typ == int:
        return fields.Integer
    else:
        return fields.String


for cls in w.walk():
    with m.class_(f"{cls.__name__}Schema", Schema) as clsname:
        schema_classes[cls] = clsname
        for name, info, metadata in w.walk_fields(cls):
            options = {"required": True}
            if info.is_optional:
                options.pop("required")

            if info.user_defined_type is None:
                m.stmt("{} = {}", name, dispatch_type(fields, info)(**options))
            else:
                params = LazyArgumentsAndKeywords(
                    args=[LazyFormat("lambda: {}()", schema_classes[info.type_])],
                    kwargs=options,
                )
                m.stmt("{} = {}({})", name, fields.Nested, params)
print(m)
