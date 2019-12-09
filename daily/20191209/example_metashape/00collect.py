from __future__ import annotations
import typing as t
import conf
from metashape.runtime import get_walker
from dictknife import loading

w = get_walker(conf.Toplevel)


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


for cls in w.walk():
    loading.dumpfile(collect(cls))
