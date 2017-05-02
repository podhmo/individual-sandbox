import yaml
import sys


def _represent_str(dumper, instance):
    if "\n" in instance:
        return dumper.represent_scalar('tag:yaml.org,2002:str', instance, style='|')
    else:
        return dumper.represent_scalar('tag:yaml.org,2002:str', instance)

yaml.add_representer(str, _represent_str)

data = yaml.load(sys.stdin)
yaml.dump(data, sys.stdout)
