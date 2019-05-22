import jsonschema
import yaml
from handofcats import as_command


@as_command
def run(*, src: str, schema: str) -> None:
    with open(schema) as rf:
        schema = yaml.load(rf)
    with open(src) as rf:
        data = yaml.load(rf)
    jsonschema.Draft4Validator.check_schema(schema)
    validator = jsonschema.Draft4Validator(schema)
    for err in validator.iter_errors(data):
        print("E", err)
        print(vars(err))
