from __future__ import annotations
import typing as t
from collections import namedtuple
from functools import lru_cache
from prestring.python.codeobject import Module, Symbol
from metashape.outputs.openapi import types
from metashape.typeinfo import typeinfo

Pair = namedtuple("Pair", "type, format")


class PyModuleLookup:
    def __init__(self) -> None:
        self.pair_to_pytype_map: t.Dict[Pair, t.Type[t.Any]] = {}
        self.pytype_to_module_path_map: t.Dict[t.Type[t.Any], str] = {}

    def register(
        self,
        pair: Pair,
        pytype: t.Type[t.Any],
        *,
        module: t.Optional[t.ModuleType] = None,
    ) -> None:
        self.pair_to_pytype_map[pair] = pytype
        if module is not None:
            # use t.NewType, module is needed. (TODO: validation)
            self.pytype_to_module_path_map[pytype] = module

    def lookup_pytype(self, pair: Pair) -> t.Optional[t.Type[t.Any]]:
        return self.pair_to_pytype_map.get(pair)

    def lookup_module(self, pytype: t.Type[t.Any]) -> t.Optional[t.ModuleType]:
        return self.pytype_to_module_path_map.get(pytype)


module_lookup = PyModuleLookup()
module_lookup.register(Pair(type="number", format=None), types.number, module=types)
module_lookup.register(Pair(type="number", format="float"), types.float)
module_lookup.register(Pair(type="number", format="double"), types.double, module=types)
module_lookup.register(Pair(type="integer", format=None), int, module=types)
module_lookup.register(Pair(type="integer", format="int32"), types.int32, module=types)
module_lookup.register(Pair(type="integer", format="int64"), types.int64, module=types)
module_lookup.register(Pair(type="string", format=None), str)
module_lookup.register(Pair(type="boolean", format=None), bool)
module_lookup.register(Pair(type="string", format="uuid"), types.uuid, module=types)
module_lookup.register(
    Pair(type="string", format="date-time"), types.date_time, module=types
)
module_lookup.register(Pair(type="string", format="date"), types.date, module=types)
module_lookup.register(Pair(type="string", format="email"), types.email, module=types)
module_lookup.register(Pair(type="string", format="uri"), types.uri, module=types)
module_lookup.register(Pair(type="array", format=None), t.List)
module_lookup.register(Pair(type="object", format=None), t.Dict)


class Resolver:
    def __init__(
        self, *, m: Module, module_lookup: PyModuleLookup = module_lookup,
    ) -> None:
        self.m = m
        self.module_lookup = module_lookup
        # todo
        # self.module_aliases: t.Dict[str, str] = {"typing": "t", "typing_extensions": "tx"}

    # todo: customizable
    def resolve_pytype(self, d: t.Dict[str, t.Any]) -> t.Type[t.Any]:
        pair = Pair(type=d.get("type"), format=d.get("format"))
        pytype = self.module_lookup.lookup_pytype(pair)
        assert pytype is not None
        return pytype

    def resolve_pytype_str_from_ref(self, ref: str) -> Symbol:
        return Symbol(ref.rsplit("/", 1)[-1])

    @lru_cache(maxsize=256)
    def resolve_pytype_str(
        self, typ: t.Type[t.Any], *, nonetype: t.Type[t.Any] = type(None)
    ) -> Symbol:
        if typ.__module__ == "builtins":
            if typ.__name__ == "NoneType":
                return "None"
            else:
                return Symbol(typ.__name__)

        info = typeinfo(typ)
        if info.is_optional:
            return Symbol(f"t.Optional[{self.resolve_pytype_str(info.type_)}]")
        elif info.is_newtype:
            mod = self.resolve_module(info.raw)
            sym = self.m.toplevel.from_(mod).import_(info.raw.__name__)
            return sym
        raise NotImplementedError(typ)

    def resolve_module(self, typ: t.Type[t.Any]) -> str:
        mod = self.module_lookup.lookup_module(typ)
        if mod is not None:
            return mod.__name__
        return typ.__module__


class DefinitionGenerator:
    def __init__(
        self, definitions: t.Dict[str, t.Any], *, m: Module, resolver: Resolver
    ) -> None:
        self.definitions = definitions
        self.resolver = resolver
        self.m = m

    def generate(self, name: str, d: t.Dict[str, t.Any]) -> None:
        if "$ref" in d:
            return  # todo
        type_ = d.get("type", "object")
        if type_ == "object":
            return self._generate_object(name, d)
        elif type_ == "array":
            return self._generate_array(name, d)
        else:
            return  # todo

    def _generate_object(self, name: str, d: t.Dict[str, t.Any]) -> None:
        m = self.m
        resolver = self.resolver

        required_set = set(d.get("required") or [])
        with m.class_(name):
            if "description" in d:
                m.docstring(d["description"])

            if not d.get("properties"):
                m.stmt("pass")
                return

            for name, prop in d["properties"].items():
                pytype = resolver.resolve_pytype(prop)
                if name not in required_set:
                    pytype = t.Optional[pytype]  # type: ignore
                m.append(f"{name}: {resolver.resolve_pytype_str(pytype)}")
                if "description" not in prop:
                    m.stmt("")
                else:
                    field = m.toplevel.from_("metashape.declared").import_("field")
                    metadata = {"description": prop["description"]}
                    m.stmt(f" = {field}(metadata={metadata})")

    def _generate_array(self, name: str, d: t.Dict[str, t.Any]) -> None:
        import json

        m = self.m
        resolver = self.resolver
        if "description" in d:
            m.stmt(f"# {name} : {d['description']}")

        if "items" not in d:
            m.stmt(f"# todo: support {json.dumps(d)}")
            m.sep()
            return
        if "$ref" not in d["items"]:
            m.stmt(f"# todo: support {json.dumps(d)}")
            m.sep()
            return

        pytype_str = resolver.resolve_pytype_str_from_ref(
            d["items"]["$ref"]
        )  # todo: lazy
        m.stmt(f"{name} = t.List[{pytype_str}]")
        m.sep()
