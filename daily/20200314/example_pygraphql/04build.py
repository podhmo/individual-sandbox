import typing as t
from prestring.python import PythonModule, Symbol
from prestring.codeobject import CodeObjectModuleMixin
from detector import Detector, Result, Object, generate_annotations, TypeInfo


class Module(PythonModule, CodeObjectModuleMixin):
    pass


def get_name_map(result: Result, *, name: str):
    annotations = generate_annotations(result, toplevel_name=name)
    return {k: v.get("after", v["before"])["name"] for k, v in annotations.items()}


def to_graphql_type(
    info: TypeInfo, *, mapping: t.Dict[t.Type[t.Any], t.Type[t.Any]], g: Symbol
) -> t.Any:
    if hasattr(info, "base") and info.base is t.Optional:
        return mapping[info.item.type]
    return g.GraphQLNonNull(mapping[info.type])


def emit(result: Result, *, m: t.Optional[Module] = None) -> Module:
    m = m or Module()
    m.toplevel = m.submodule()
    g = m.toplevel.import_("graphql", as_="g")
    m.sep()

    mapping = {
        str: g.GraphQLString,
        int: g.GraphQLInt,
        bool: g.GraphQLBool,
    }

    name_map = get_name_map(result, name="Person")
    type_map = {name: g.symbol(name) for name in name_map.values()}

    for info in result.history:
        if not isinstance(info, Object):
            continue
        name = name_map["/".join(info.path)]
        m.stmt("{} = {}(", name, g.GraphQLObjectType)
        with m.scope():
            m.stmt("{!r},", name)
            m.stmt("lambda: {")
            with m.scope():
                for fieldname, field in info.props.items():
                    m.stmt(
                        "{!r}: {},",
                        fieldname,
                        g.GraphQLField(to_graphql_type(field, mapping=mapping, g=g)),
                    )
            m.stmt("}")
        m.stmt(")")
    return m


data = [{"name": "foo", "age": 20}, {"name": "boo", "age": "20", "nickname": "B"}]
result = Result()
Detector().detect_dict_many(data, path=[], result=result)
print(emit(result))
