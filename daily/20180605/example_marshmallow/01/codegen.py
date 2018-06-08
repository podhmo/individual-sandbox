import re
from swagger_marshmallow_codegen.driver import Driver
from swagger_marshmallow_codegen.resolver import Resolver
from swagger_marshmallow_codegen.langhelpers import normalize


def snakecase(
    name, rx0=re.compile('(.)([A-Z][a-z]+)'), rx1=re.compile('([a-z0-9])([A-Z])'), separator="_"
):
    pattern = r'\1{}\2'.format(separator)
    return rx1.sub(pattern, rx0.sub(pattern, name)).lower()


class MyResolver(Resolver):
    def resolve_normalized_name(self, name):
        return snakecase(normalize(name))


class MyDriver(Driver):
    resolver_factory = MyResolver
