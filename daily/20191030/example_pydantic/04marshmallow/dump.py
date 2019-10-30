import handofcats
from dictknife import loading
from marshmallow_jsonschema import JSONSchema
from config import Config


@handofcats.as_command
def run():
    toplevel = JSONSchema().dump(Config())
    loading.dumpfile(toplevel)
