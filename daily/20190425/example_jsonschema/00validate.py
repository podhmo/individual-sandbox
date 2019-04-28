from handofcats import as_command
import jsonschema
from dictknife import loading


@as_command
def run(*, src: str, schema: str) -> None:
    src = loading.loadfile(src)
    schema = loading.loadfile(schema)
    validator = jsonschema.Draft4Validator(schema)
    print("@", validator.validate(src))
