import handofcats
from pydantic.schema import schema
from dictknife import loading
from config import Config


@handofcats.as_command
def run():
    toplevel = schema([Config], title="App Config")
    toplevel["$ref"] = "#/definitions/Config"
    loading.dumpfile(toplevel)
