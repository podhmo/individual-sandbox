from tempfile import TemporaryDirectory
from pathlib import Path
import dictknife.loading as loading
from dictknife.pp import pp, indent


with TemporaryDirectory() as d:
    d = Path(d)
    data = {
        "definitions": {
            "person": {
                "$ref": "./models/person.json#/definitions/person"
            }
        }
    }
    loading.dumpfile(data, str(d.joinpath("./swagger.json")))

    person = {
        "definitions": {
            "person": {
                "properties": {
                    "name": {
                        "$ref": "../primitives/name.json"
                    },
                    "age": {
                        "$ref": "../primitives/age.json"
                    }
                }
            }
        }
    }
    loading.dumpfile(person, str(d.joinpath("./models/person.json")))

    name = {
        "type": "string",
    }
    loading.dumpfile(name, str(d.joinpath("./primitives/name.json")))
    age = {
        "type": "integer",
        "minimum": 0,
    }
    loading.dumpfile(age, str(d.joinpath("./primitives/age.json")))

    # raw
    print("raw")
    with d.joinpath("swagger.json").open() as rf:
        data = loading.load(rf)
        with indent(3):
            pp(data)

    # resolve
    print("\nresolve json ref")
    with d.joinpath("swagger.json").open() as rf:
        from dictknife.jsonknife.resolver import get_resolver_from_filename
        from dictknife.jsonknife import Expander
        resolver = get_resolver_from_filename(rf.name)
        expander = Expander(resolver)
        data = loading.load(rf)
        pp(expander.expand())

    # bundle
    print("\nuse individual resolution")
    with d.joinpath("swagger.json").open() as rf:
        from dictknife.jsonknife.resolver import get_resolver_from_filename
        from dictknife.jsonknife import Bundler
        resolver = get_resolver_from_filename(rf.name)
        bundler = Bundler(resolver)
        data = loading.load(rf)
        pp(bundler.bundle())
