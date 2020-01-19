import typing as t
import json
import argparse
from pydantic.error_wrappers import ValidationError
from config import Config


def ConfigType(filename_or_content: str, *, prefix: str = "file://") -> Config:
    try:
        if filename_or_content.startswith(prefix):
            with open(filename_or_content[len(prefix) :]) as rf:
                d = json.load(rf)
        else:
            d = json.loads(filename_or_content)
    except (ValueError, FileNotFoundError) as e:
        raise argparse.ArgumentTypeError(str(e))

    try:
        return Config(**d)
    except ValidationError as e:
        raise argparse.ArgumentTypeError(str(e))


parser = argparse.ArgumentParser()
parser.add_argument("--config", type=ConfigType, required=False)
parser.add_argument("--show-schema", action="store_true")
args = parser.parse_args()


if args.show_schema:
    from pydantic.schema import schema

    toplevel_schema = schema([Config])
    print(json.dumps(toplevel_schema, indent=2, ensure_ascii=False))
    parser.exit()


def use(config: "Config") -> None:
    print(config)


use(args.config)
