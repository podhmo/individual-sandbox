import json
import toml


def run(*, src: str, format: str) -> None:
    import sys
    module = getattr(sys.modules[__name__], format)
    with open(src) as rf:
        d = json.load(rf)
    print(module.dumps(d).rstrip(), file=sys.stdout)


def main(argv=None):
    import argparse
    parser = argparse.ArgumentParser(description=None)
    parser.print_usage = parser.print_help

    parser.add_argument('src')
    parser.add_argument("--format", choices=["json", "toml"], default="json")

    args = parser.parse_args(argv)
    run(**vars(args))


if __name__ == '__main__':
    main()
