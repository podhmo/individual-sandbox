import typing as t
import sys
import argparse
import json


def JSONDictType(
    filename_or_content: str, *, prefix: str = "file://"
) -> t.Dict[str, t.Any]:
    try:
        if filename_or_content.startswith(prefix):
            with open(filename_or_content[len(prefix) :]) as rf:
                d: t.Dict[str, t.Any] = json.load(rf)
        else:
            d = json.loads(filename_or_content)
        return d
    except (ValueError, FileNotFoundError) as e:
        raise argparse.ArgumentTypeError(str(e))


parser = argparse.ArgumentParser()
parser.add_argument("--config", type=JSONDictType)

args = parser.parse_args()
json.dump(args.config, sys.stdout, indent=2)
print("")
