

def hello(*, name="world") -> str:
    return f"hello, {name}"


def main(argv=None):
    import argparse

    parser = argparse.ArgumentParser(prog=hello.__name__, description=hello.__doc__)
    parser.print_usage = parser.print_help  # type: ignore
    parser.add_argument('--name', required=False, default='world', help='-')
    args = parser.parse_args(argv)
    params = vars(args).copy()
    return hello(**params)


if __name__ == '__main__':
    main()
