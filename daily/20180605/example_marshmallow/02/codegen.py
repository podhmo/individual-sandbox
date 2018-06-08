import re
from swagger_marshmallow_codegen.driver import Driver
from swagger_marshmallow_codegen.accessor import Accessor


def snakecase(
    name, rx0=re.compile('(.)([A-Z][a-z]+)'), rx1=re.compile('([a-z0-9])([A-Z])'), separator="_"
):
    pattern = r'\1{}\2'.format(separator)
    return rx1.sub(pattern, rx0.sub(pattern, name)).lower()


class CustomAccessor(Accessor):
    def update_options_pre_properties(self, d, opts):
        opts = super().update_options_pre_properties(d, opts)
        for name in self.properties(d):
            opts[name]["attribute"] = snakecase(name)
        return opts


class MyDriver(Driver):
    accessor_factory = CustomAccessor
