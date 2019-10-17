import typing as t
import typing_extensions as tx
import typing_inspect

from metashape.flavors.openapi.resolve import resolve_enum

JSONSchemaType = tx.Literal["boolean", "string", "integer", "number", "object", "array"]


def run(typ):
    print(typ, resolve_enum(typ))


run(int)
run(JSONSchemaType)
