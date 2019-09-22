import json
import re
import sys


def run(*, use_json: bool = False):
    rx = re.compile(r"(INFO|WARNING|ERROR)", re.IGNORECASE)
    for i, line in enumerate(sys.stdin):
        m = rx.search(line)
        if m is None:
            continue
        if use_json:
            print(json.dumps({"line": i + 1, "status": m.group(0), "message": line}))
        else:
            print(f"{m.group(0)}:{i+1}:{line}", end="")


def main(argv=None):
    import argparse

    parser = argparse.ArgumentParser(description=None)
    parser.print_usage = parser.print_help
    parser.add_argument("--json", action="store_true", dest="use_json")
    args = parser.parse_args(argv)
    run(**vars(args))


if __name__ == "__main__":
    main()
