import jsonschema
from handofcats import as_command
from dictknife import loading


@as_command
def run(filename: str) -> None:
    schema = loading.loadfile(filename)
    d = {
        "config": {
            "variables": {"env": "production"},
            "providers": {"aws": {"region": "us-west-1"}},
        }
    }
    d = {
        "config": {
            "variables": {"env": "production"},
            "providers": {"aws": {"region": "us-west-2"}},
        }
    }

    cls = jsonschema.Draft4Validator
    cls.check_schema(schema)
    for e in cls(schema).iter_errors(d):
        print("cause?")
        print("	context", e.context)
        print("	cause", e.cause)
        print("where?")
        print("	instance_path", e.path)
        print("	schema_path", e.schema_path)
        print("	validator", e.validator)
