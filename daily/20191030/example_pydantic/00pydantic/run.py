import pydantic
import handofcats
from config import Config


@handofcats.as_command
def run(*, config: str) -> None:
    import sys
    from dictknife import loading

    d = loading.loadfile(config)
    try:
        c = Config(**d)
    except pydantic.ValidationError as e:
        print(e.json(), file=sys.stderr)
        sys.exit(1)
    print(c)
