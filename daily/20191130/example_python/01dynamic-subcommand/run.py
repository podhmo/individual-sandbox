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

    actions = {}
    # foo sub-command
    actions["foo"] = foo_parser = argparse.ArgumentParser("foo")
    foo_parser.add_argument("-x")
    foo_parser.add_argument("-y")

    # bar sub-command
    actions["bar"] = bar_parser = argparse.ArgumentParser("bar")
    bar_parser.add_argument("-z")

    args = actions[action_name].parse_args(rest)
    return globals()[action_name](**vars(args))


if __name__ == "__main__":
    main()
