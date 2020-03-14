import typing as t
from handofcats import as_subcommand


@as_subcommand
def scan(filename: str) -> None:
    from dictknife import loading
    from detector import show

    d = loading.loadfile(filename)
    show(d)


@as_subcommand
def annotate(filename: str, *, use_fullname: bool = False) -> None:
    from dictknife import loading
    from detector import detect, generate_annotations

    d = loading.loadfile(filename)
    result = detect(d)
    r = generate_annotations(
        result, use_fullname=use_fullname, toplevel_name="toplevel"
    )
    loading.dumpfile(r)


@as_subcommand
def emit(filename: str) -> None:
    from dictknife import loading
    from prestring.python import Module
    from inflection import pluralize, singularize, camelize
    from detector import detect, Object, TypeInfo

    name_map = {}
    m = Module()
    m.toplevel = m.submodule()
    m.sep()

    def _pytype(info: TypeInfo, *, m=m, aliases: t.Dict[str, str] = {"typing": "t"}):
        if hasattr(info, "base"):
            module = aliases.get(info.base.__module__) or info.base.__module__
            m.toplevel.import_(info.base.__module__, aliases.get(info.base.__module__))
            if info.base is t.Optional:
                return f"{module}.Optional[{_pytype(info.item)}]"
            elif info.base is t.List:
                return f"{module}.List[{_pytype(info.item)}]"
        elif hasattr(info, "type"):
            module = aliases.get(info.type.__module__) or info.type.__module__
            prefix = module + "."
            if module == "builtins":
                prefix = ""
            else:
                m.toplevel.import_(
                    info.type.__module__, aliases.get(info.type.__module__)
                )
            return prefix + info.type.__name__
        return name_map[id(info)]

    d = loading.loadfile(filename)
    result = detect(d)
    for info in result.history:
        if isinstance(info, Object):
            # todo: namedict
            name = info.path[-1] if info.path else "toplevel"
            if name.startswith(":"):
                name = info.path[-2]
            if pluralize(name) == name:
                name = singularize(name)

            name = name_map[id(info)] = camelize(name)
            with m.class_(name):
                for name, sub_info in info.props.items():
                    m.stmt("{}: {}", name, _pytype(sub_info))
    print(m)


as_subcommand.run()
