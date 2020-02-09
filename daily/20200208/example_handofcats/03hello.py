import json
from handofcats import as_command, Config


@as_command(config=Config(cont=lambda x: print(json.dumps(x))))
def hello(*, name="world") -> str:
    return f"hello, {name}"
