import jsonschema
from dictknife import loading
from handofcats import as_command


@as_command
def run(*, schema: str, data: str) -> None:
    data = loading.loadfile(data)
    schema = loading.loadfile(schema)
    jsonschema.validate(data, schema)  # skip check schema?
