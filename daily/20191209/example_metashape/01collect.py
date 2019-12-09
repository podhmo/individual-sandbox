from __future__ import annotations
import typing as t
import conf
from metashape.runtime import get_walker
from metashape.analyze.walker import Walker
from metashape.analyze.collector import Collector
from dictknife import loading


@Collector
def collect(cls: t.Type[t.Any], *, w: t.Optional[Walker]):
    props = {}
    for name, typeinfo, metadata in w.for_type(cls).walk():
        fieldname = w.resolver.metadata.resolve_name(metadata, default=name)
        props[fieldname] = collect(getattr(cls, name), w=w)
    return props


w = get_walker(conf.Toplevel)
for cls in w.walk():
    loading.dumpfile(collect(cls, w=w))
