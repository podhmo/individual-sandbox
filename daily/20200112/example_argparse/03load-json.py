import typing as t
import argparse


def as_json(filename: str) -> t.Dict[str, t.Any]:
    import json

    try:
        with open(filename) as rf:
            return json.load(rf)
    except Exception as e:
        raise argparse.ArgumentTypeError(str(e))


parser = argparse.ArgumentParser(fromfile_prefix_chars="@")
parser.add_argument("data", type=as_json)
args = parser.parse_args(["data.json"])
print(args)

print(
    """
----------------------------------------
"""
)

parser = argparse.ArgumentParser(fromfile_prefix_chars="@")
parser.add_argument("data", type=as_json)
args = parser.parse_args(["data.jsonnn"])
print(args)

print(
    """
----------------------------------------
"""
)

parser = argparse.ArgumentParser(fromfile_prefix_chars="@")
parser.add_argument("data", type=as_json)
args = parser.parse_args(["data-invalid.json"])
print(args)
