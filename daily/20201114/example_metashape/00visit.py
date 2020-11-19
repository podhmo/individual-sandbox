from __future__ import annotations
import typing as t
import logging
import dataclasses
from handofcats import as_command
import dictknife
from dictknife import loading
from dictknife.langhelpers import make_dict

# TODO: flatten
# TODO: normalize name
# TODO: metashape like structure
# TODO: support schemas
# - object
# - primitive

AnyDict = t.Dict[str, t.Any]
logger = logging.getLogger(__name__)


class Resolver:
    def __init__(self, fulldata: AnyDict) -> None:
        self.fulldata = fulldata
        self._accessor = dictknife.Accessor()  # todo: rename

    def resolve_python_type(self, d: AnyDict, *, name: str,) -> t.Type[t.Any]:
        type_hints = {}
        for field_name, field in d["properties"].items():
            type_hints[field_name] = str  # TODO: guess type
        return type(name, (), {"__annotations__": type_hints})

    ##

    def has_ref(self, d: AnyDict) -> bool:
        return "$ref" in d

    def has_allof(self, d: AnyDict) -> bool:
        return "allOf" in d

    def has_schema(
        self, d: AnyDict, cand: t.Tuple[str, ...] = ("object",), fullscan: bool = True,
    ) -> bool:
        typ = d.get("type", None)
        if typ in cand:
            return True
        if "properties" in d:
            return True
        if self.has_allof(d):
            return True
        if not self.has_ref(d):
            return False
        if not fullscan:
            return False

        # TODO: cache?
        _, definition = self.resolve_ref_definition(d)
        return self.has_schema(definition, fullscan=False)

    ##
    def resolve_ref_definition(
        self,
        d: t.Dict[str, t.Any],
        name: t.Optional[str] = None,
        i: int = 0,
        level: int = -1,
    ) -> t.Tuple[str, t.Dict[str, t.Any]]:
        # return schema_name, definition_dict
        # todo: support quoted "/"
        # on array
        if "items" in d:
            definition = d
            name, _ = self.resolve_ref_definition(
                d["items"], name=name, i=i, level=level + 1
            )  # xxx
            return name, definition

        if "$ref" not in d:
            return self.resolve_schema_name(name), d
        if level == 0:
            return self.resolve_schema_name(name), d

        logger.debug("    resolve: %sref=%r", "  " * i, d["$ref"])

        path = d["$ref"][len("#/") :].split("/")
        name = path[-1]

        parent = self._accessor.maybe_access_container(path)
        if parent is None:
            logger.warning("%r is not found", d["$ref"])
            return self.resolve_schema_name(name), d
        ref_name, definition = self.resolve_ref_definition(
            parent[name], name=name, i=i + 1, level=level - 1
        )

        # import for separated output
        # if X_MARSHMALLOW_INLINE not in definition:
        #     if c is not None and (
        #         "properties" in definition
        #         or (
        #             (
        #                 "additionalProperties" in definition
        #                 or "items" in definition
        #                 or "allOf" in definition
        #             )
        #             and self.has_schema(fulldata, definition)
        #         )
        #     ):
        #         c.relative_import_from_lazy(ref_name)
        return ref_name, definition


class Accessor:
    def __init__(self, resolver: t.Optional[Resolver] = None):
        self.resolver = resolver or Resolver()

    def schemas(self, d: AnyDict) -> t.Iterator[t.Tuple[str, AnyDict]]:
        try:
            components = d["components"]
        except KeyError:
            logger.info("skip, components is not found")
            return []
        try:
            schemas = components["schemas"]
        except KeyError:
            logger.info("skip, components/schemas is not found")
            return []

        for k, v in schemas.items():
            yield k, v


@dataclasses.dataclass
class Context:
    types: AnyDict = dataclasses.field(default_factory=make_dict)


def visit(ctx: Context, d: AnyDict) -> None:
    a = Accessor(Resolver(d))
    for name, sd in a.schemas(d):
        # TODO: normalize
        ctx.types[name] = a.resolver.resolve_python_type(name, sd)


def emit(ctx: Context):
    from prestring.python import Module

    m = Module()
    for name, cls in ctx.types.items():
        with m.class_(name):
            # TODO: omit class inheritance
            for field_name, field_type in t.get_type_hints(cls).items():
                # TODO: to pytype
                m.stmt(f"{field_name}: {field_type.__name__}")
    return m


@as_command
def run(filename: str) -> None:
    d = loading.loadfile(filename)
    ctx = Context()
    visit(ctx, d)
    print("@", ctx)
    print("----------------------------------------")
    print(emit(ctx))
