import typing as t
import argparse
import json


def run(*, data: t.Dict[str, t.Any]) -> None:
    print(data)
    # loading.dumpfile(data)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--data", type=lambda x: json.load(argparse.FileType("r")(x)))
    args = parser.parse_args()
    return run(**vars(args))


if __name__ == "__main__":
    main()
