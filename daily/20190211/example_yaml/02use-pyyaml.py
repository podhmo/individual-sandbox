import sys
import yaml
from handofcats import as_command


@as_command
def run(*, src: str) -> None:
    with open(src) as rf:
        data = yaml.load(rf)
    yaml.dump(data, sys.stdout, default_flow_style=False, allow_unicode=True)
