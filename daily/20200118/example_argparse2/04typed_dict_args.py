import typing as t
import typing_extensions as tx
import sys
import argparse
import json

_XXXConfigDict = tx.TypedDict("_XXXConfigDict", {"token": str})
_ZZZConfigDict = tx.TypedDict("_ZZZConfigDict", {"clientId": str, "clientSecret": str})

_MainConfigDict = tx.TypedDict("_MainConfigDict", {"db": str})
_ThirdpartyConfigDict = tx.TypedDict(
    "_ThirdpartyConfigDict", {"xxx": _XXXConfigDict, "zzz": _ZZZConfigDict,},
)


class _ConfigDictOptional(tx.TypedDict, total=False):
    thirdparty: _ThirdpartyConfigDict


class ConfigDict(_ConfigDictOptional, total=True):
    main: _MainConfigDict


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


def ConfigDictType(filename_or_content: str, *, prefix: str = "file://") -> ConfigDict:
    d = JSONDictType(filename_or_content, prefix=prefix)
    return t.cast(ConfigDict, d)


parser = argparse.ArgumentParser()
parser.add_argument("--config", type=ConfigDictType)

args = parser.parse_args()
json.dump(args.config, sys.stdout, indent=2)
print("")
