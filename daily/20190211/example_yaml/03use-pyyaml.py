import sys
import re
import yaml
from handofcats import as_command


@as_command
def run(*, src: str) -> None:
    def _represent_str(dumper, instance, _rx=re.compile("[\n#:]")):
        m = _rx.search(instance)
        if m is None:
            style = None
        elif m.group(0) == "#" or m.group(0) == ":":
            style = "'"
        else:
            style = "|"
        return dumper.represent_scalar("tag:yaml.org,2002:str", instance, style=style)

    yaml.Dumper.add_representer(str, _represent_str)

    with open(src) as rf:
        data = yaml.load(rf)
    yaml.dump(data, sys.stdout, default_flow_style=False, allow_unicode=True)
