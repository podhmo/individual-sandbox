import marshmallow
import handofcats
from config import Config


@handofcats.as_command
def run(*, config: str) -> None:
    import sys
    from dictknife import loading

    d = loading.loadfile(config)
    try:
        c = Config().load(d)
    except marshmallow.ValidationError as e:
        print(e.normalized_messages(), file=sys.stderr)
        sys.exit(1)
    print(c)
