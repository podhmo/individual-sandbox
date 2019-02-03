import sys
import argparse
import json
import toml


def run(*, src: str, format: str) -> None:
    module = getattr(sys.modules[__name__], format)
    with open(src) as rf:
        d = json.load(rf)
    print(module.dumps(d).rstrip(), file=sys.stdout)


def create_extra_parser_mapping():
    mapping = {}

    # json
    mapping["json"] = parser = argparse.ArgumentParser("extra")
    parser.add_argument("--sort-keys", action="store_true")

    # toml
    mapping["toml"] = parser = argparse.ArgumentParser("extra")
    return mapping


def main(argv=None):
    parser = argparse.ArgumentParser(description=None)
    parser.print_usage = parser.print_help
    parser.epilog = "extra arguments:"

    parser.add_argument('src')
    parser.add_argument("--format", choices=["json", "toml"], default="json")

    args, rest = parser.parse_known_args(argv)
    rest = [(x[7:] if x.startswith("--extra") else x) for x in rest]
    extra = vars(create_extra_parser_mapping()[args.format].parse_args(rest))

    print("RRRR", rest, "@", extra, file=sys.stderr)
    run(**vars(args))


if __name__ == '__main__':
    main()
