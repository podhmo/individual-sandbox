import jsonschema
import yaml
from handofcats import as_command
from dictknife import Accessor
from loader import Loader, mem


@as_command
def run(*, src: str, schema: str) -> None:
    with open(schema) as rf:
        schema = yaml.load(rf)
    with open(src) as rf:
        loader = Loader(rf)
        try:
            assert loader.check_data()
            data = loader.get_data()
        finally:
            loader.dispose()

    jsonschema.Draft4Validator.check_schema(schema)
    validator = jsonschema.Draft4Validator(schema)
    for err in validator.iter_errors(data):
        print("E", err)
        a = Accessor()
        path = list(err.path)
        ob = a.access(data, path[:-1])

        ev = mem[id(ob)]

        for kev, vev in ev.value:
            if kev.value == path[-1]:
                print("----------------------------------------")
                print(str(vev.start_mark).lstrip())
                lineno = vev.start_mark.line + 1
                with open(src) as rf:
                    for i, line in enumerate(rf, 1):
                        if lineno == i:
                            print(f"  {i:02d}: -> {line}", end="")
                        else:
                            print(f"  {i:02d}:    {line}", end="")
                break
