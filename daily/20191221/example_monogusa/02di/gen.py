import typing as t
import inspect
from types import ModuleType
from monogusa.dependencies import scan_module
from monogusa.web.codegen._fnspec import fnspec, Fnspec
from monogusa.web.codegen._codeobject import codeobject, Object, Module
from prestring.utils import LazyArguments


def create_component_code(spec: Fnspec, spec_map: t.Dict[str, Fnspec]) -> Object:
    @codeobject
    def _component_code(m: Module, name: str) -> Module:
        args = []
        for name, val, kind in spec.arguments:
            if len(spec_map[name].arguments) == 0:
                args.append(
                    f"{name}: {spec.type_str_of(val)}=Depends({spec_map[name].fullname})"
                )
            else:
                # todo: deps
                args.append(f"{name}: {spec.type_str_of(val)}=Depends({name})")

        with m.def_(spec.name, LazyArguments(args), return_type=spec.return_type):
            # xxx: async
            is_coroutine = inspect.iscoroutinefunction(spec.body)
            if is_coroutine:
                m.body.body[-3].fmt = f"async {m.body.body[-3].fmt}"

            m.return_(
                "{}({})",
                spec.fullname,
                LazyArguments([name for name, _, _ in spec.arguments]),
            )
            # xxx: await
            if is_coroutine:
                m.body.body[-2].fmt = m.body.body[-2].fmt.replace(
                    "return ", "return await "
                )
        return m

    _component_code.name == spec.name
    _component_code.__name__ = spec.name
    return _component_code


def emit_components(m: Module, *, target_module: ModuleType) -> Module:
    m.toplevel.from_("fastapi", "Depends")

    scanned = scan_module(target_module)
    spec_map = {
        component.__name__: fnspec(component) for component in scanned.components
    }
    code_map: t.Dict[str, Object] = {}

    seen: t.Set(str) = set()
    for name, spec in spec_map.items():
        if inspect.iscoroutinefunction(spec.body):
            raise NotImplementedError("async support")

        if len(spec.arguments) > 0:
            code = create_component_code(spec, spec_map)
            code_map[name] = code

    def _visit(name: str) -> None:
        if name in seen:
            return
        seen.add(name)

        if name not in code_map:
            return

        spec = spec_map[name]
        for subname, _, _ in spec.arguments:
            if subname not in code_map:
                continue
            _visit(subname)

        code = code_map[name]
        m.stmt(code)

    # emit with dependencies ordered
    for name in spec_map:
        _visit(name)

    return m


import commands  # noqa F401

m = Module()
m.toplevel = m.submodule()
m = emit_components(m, target_module=commands)
print(m)
