import jsonschema
import pickle
from dictknife import loading
from handofcats import as_command


@as_command
def run(*, schema: str, data: str, cache: str) -> None:
    data = loading.loadfile(data)

    try:
        with open(cache) as rf:
            validator = pickle.load(rf)
    except FileNotFoundError:
        schema = loading.loadfile(schema)
        # jsonschema.Draft7Validator.check_schema(schema)
        validator = jsonschema.Draft7Validator(schema)
        with open(cache, "wb") as wf:
            pickle.dump(validator, wf)
    print(list(validator.iter_errors(data)))
