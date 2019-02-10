import logging
from dictknife.jsonknife import get_resolver
from dictknife.swaggerknife.migration import Migration
from handofcats import as_command


@as_command
def run(*, src: str) -> None:
    logging.basicConfig(level=logging.DEBUG)

    resolver = get_resolver(src)
    with Migration(resolver).migrate() as u:
        for k, item in u.iterate_items():
            if k == "definitions/person":
                ref = "#/definitions/person/properties/value"
                u.update(item.resolver, ref, {"type": "integer"})
            if k == "definitions/name":
                ref = "#/definitions/name/description"
                u.update(item.resolver, ref, "name of something")
