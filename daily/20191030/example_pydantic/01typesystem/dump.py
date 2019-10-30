import handofcats
import typesystem
from dictknife import loading
from config import Config


@handofcats.as_command
def run() -> None:
    toplevel = typesystem.to_json_schema(Config)
    loading.dumpfile(toplevel)
