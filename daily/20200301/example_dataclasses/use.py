from handofcats import as_command
from schema import Config
import magicalimport


@as_command
def run(*, config: str):
    c: Config = magicalimport.import_symbol(config, cwd=True)
    print("db url is", c.app.db)
