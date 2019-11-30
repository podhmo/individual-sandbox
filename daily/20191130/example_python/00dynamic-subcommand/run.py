def foo(*, x: str, y: str) -> None:
    print(f"foo x={x} y={y}")


def bar(*, z: str) -> None:
    print(f"bar z={z}")


def main(argv=None):
    import argparse
    import json

    first_parser = argparse.ArgumentParser()
    first_parser.add_argument("-c", "--config", type=argparse.FileType("r"))
    first_args, rest = first_parser.parse_known_intermixed_args()

    data = json.load(first_args.config)
    action_name = data["action"]

    class _SecondParser(argparse.ArgumentParser):
        def error(self, msg: str) -> None:
            import sys

            self.__class__ = argparse.ArgumentParser  # black magic
            print(msg, file=sys.stderr)
            self.parse_args([action_name, "-h"])

    second_parser = _SecondParser()
    subparsers = second_parser.add_subparsers(required=True, title="actions")

    # foo sub-command
    foo_parser = subparsers.add_parser("foo")
    foo_parser.add_argument("-x")
    foo_parser.add_argument("-y")
    foo_parser.set_defaults(action=foo)

    # bar sub-command
    bar_parser = subparsers.add_parser("bar")
    bar_parser.add_argument("-z")
    bar_parser.set_defaults(action=bar)

    second_args = second_parser.parse_args([action_name, *rest])
    params = vars(second_args)
    return params.pop("action")(**params)


if __name__ == "__main__":
    main()
