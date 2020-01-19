import typing as t
import typing_extensions as tx
import sys
import argparse
import json


class _ConfigDictOptional(tx.TypedDict, total=False):
    thirdparty: tx.TypedDict(
        "_ThirdpartyConfigDict",
        {
            "xxx": tx.TypedDict("_XXXConfigDict", {"token": str}),
            "zzz": tx.TypedDict(
                "_ZZZConfigDict", {"clientId": str, "clientSecret": str}
            ),
        },
    )


class ConfigDict(_ConfigDictOptional, total=True):
    main: tx.TypedDict("_MainConfigDict", {"db": str})


def JSONDictType(
    filename_or_content: str, *, prefix: str = "file://"
) -> t.Dict[str, t.Any]:
    try:
        if filename_or_content.startswith(prefix):
            with open(filename_or_content[len(prefix) :]) as rf:
                return json.load(rf)
        return json.loads(filename_or_content)
    except (ValueError, FileNotFoundError) as e:
        raise argparse.ArgumentTypeError(str(e))


def ConfigDictType(filename_or_content: str, *, prefix: str = "file://") -> ConfigDict:
    d = JSONDictType(filename_or_content, prefix=prefix)
    return ConfigDict(**d)


parser = argparse.ArgumentParser()
parser.add_argument("--config", type=ConfigDictType)

args = parser.parse_args()
json.dump(args.config, sys.stdout, indent=2)
print("")
