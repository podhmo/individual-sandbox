import sys
from ruamel.yaml import YAML
from handofcats import as_command


@as_command
def run(*, src: str) -> None:
    yaml = YAML(typ="rt")
    with open(src) as rf:
        data = yaml.load(rf)
    yaml.dump(data, sys.stdout)
