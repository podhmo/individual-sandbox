import typing as t
from dictknife import loading
from metashape.runtime import get_walker
from magicalimport import import_symbol

cls = import_symbol("./conf.py:Toplevel", here=__file__)
cls = import_symbol("../03nested-dataclass/conf.py:toplevel", here=__file__)
w = get_walker(cls)


def collect(
    cls: t.Union[t.Type[t.Any], t.List[t.Type[t.Any]]]
) -> t.Union[t.Dict[str, t.Any], t.List[t.Dict[str, t.Any]]]:
    props = {}
    for name, typeinfo, metadata in w.for_type(cls).walk():
        fieldname = w.resolver.metadata.resolve_name(metadata, default=name)
        if w.resolver.typeinfo.get_custom(typeinfo) is not None:
            props[fieldname] = collect(getattr(cls, name))
        else:
            val = getattr(cls, name)
            if isinstance(val, (list, tuple)):
                props[fieldname] = [collect(x) for x in val]
            else:
                props[fieldname] = val
    return props


d = {}
for cls in w.walk():
    d.update(collect(cls))
loading.dumpfile(d, format="yaml")
