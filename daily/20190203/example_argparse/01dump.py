import sys
import argparse
import json
import toml


def run(*, src: str, format: str, extra: None) -> None:
    extra = extra or {}
    module = getattr(sys.modules[__name__], format)
    with open(src) as rf:
        d = json.load(rf)
    print(module.dumps(d, **extra).rstrip(), file=sys.stdout)


class ExtraArgumentsParsers:
    def __init__(
        self,
        *,
        prefix="extra",
        parser_factory=argparse.ArgumentParser,
    ):
        self.prefix = prefix
        self.mapping = {}
        self.parser_factory = parser_factory

    def parse_args(self, name, args):
        prefix = f"--{self.prefix}"
        rest = [(x[7:] if x.startswith(prefix) else x) for x in args]
        return self.get_parser(name).parse_args(rest)

    def get_parser(self, name):
        return self.mapping[name]

    def add_parser(self, name):
        self.mapping[name] = p = self.parser_factory(
            f"{self.prefix} arguments {name}",
            description=f"for {name}",
            add_help=False,
        )
        return p

    def as_epilog(self):
        r = [f"{self.prefix} arguments: (with --{self.prefix}<option>)"]

        formatter = argparse.HelpFormatter("")
        for name, parser in self.mapping.items():
            is_empty = True
            for action_group in parser._action_groups:
                if len(action_group._group_actions) > 0:
                    is_empty = False
                    break
            if is_empty:
                continue

            formatter.start_section(parser.description)
            for action_group in parser._action_groups:
                formatter.add_arguments(action_group._group_actions)
            formatter.end_section()
        r.extend(formatter.format_help().split("\n"))
        return "\n  ".join(r)


def main(argv=None):
    import argparse
    parser = argparse.ArgumentParser(
        description=None,
        formatter_class=argparse.RawTextHelpFormatter,
    )
    parser.print_usage = parser.print_help
    parser.epilog = "extra arguments:"

    parser.add_argument('src')
    parser.add_argument("--format", choices=["json", "toml"], default="json")

    ex_parsers = ExtraArgumentsParsers()
    # json
    ex_parser = ex_parsers.add_parser("json")
    ex_parser.add_argument("--sort-keys", action="store_true", help="sort keys")
    ex_parser = None
    # toml
    ex_parser = ex_parsers.add_parser("toml")
    parser.epilog = ex_parsers.as_epilog()  # xxx
    ex_parser = None

    args, rest = parser.parse_known_args(argv)
    extra = ex_parsers.parse_args(args.format, rest)
    run(**vars(args), extra=vars(extra))


if __name__ == '__main__':
    main()
