from monogusa.cli.runtime import collect_commands
from monogusa.web.codegen import emit_routers
import commands

from handofcats import as_command


@as_command
def run() -> None:
    fns = list(collect_commands(commands))
    print(emit_routers(fns, with_main=True))
