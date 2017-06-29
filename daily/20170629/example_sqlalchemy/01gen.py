# -*- coding:utf-8 -*-
from collections import OrderedDict
from sqlalchemy.ext.automap import automap_base
from sqlalchemy import create_engine
from sqlalchemy.inspection import inspect
from dictknife import loading
import logging
logger = logging.getLogger(__name__)


class Collector:
    def __init__(self, resolver):
        self.resolver = resolver

    def collect(self, classes):
        d = OrderedDict()
        for c in classes:
            mapper = inspect(c)
            d[mapper.local_table.fullname] = self._collect_from_mapper(mapper)
        return d

    def _collect_from_mapper(self, m):
        d = OrderedDict()
        for prop in m.iterate_properties:
            if hasattr(prop, "direction"):
                pairs = prop.synchronize_pairs
                assert len(pairs) == 1, "multi keys are not supported"
                d[prop.key] = {
                    "table": prop.target.fullname,
                    "direction": prop.direction.name,
                    "uselist": prop.uselist,
                    "relation": {
                        "to": "{}.{}".format(pairs[0][0].table.fullname, pairs[0][0].name),
                        "from": "{}.{}".format(pairs[0][1].table.fullname, pairs[0][1].name),
                    }
                }
            else:
                assert len(prop.columns) == 1, "multi keys are not supported"
                c = prop.columns[0]
                d[prop.key] = {
                    "type": self.resolver.resolve_type(c),
                    "nullable": c.nullable,
                }
        return d


class Resolver:
    mapping = {int: "Integer", str: "String"}

    def __init__(self, mapping=None):
        self.mapping = mapping or self.__class__.mapping

    def resolve_type(self, c):
        if c.primary_key:
            return "ID"
        typ = self.mapping.get(c.type.python_type)
        if typ is not None:
            return typ
        else:
            logger.info("unexpected column: %s", c)
            return c.type.python_type.__name__  # xxx


def main(src):
    Base = automap_base()
    engine = create_engine(src)
    Base.prepare(engine, reflect=True)
    collector = Collector(Resolver())
    d = collector.collect(Base.classes)
    loading.dumpfile(d, format="json")


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--src", default="sqlite:///./dog.db")
    args = parser.parse_args()
    main(args.src)
