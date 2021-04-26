

def hello(*, name: str) -> None:
    print(f"hello {name}")


def main(argv=None):
    import argparse

    parser = argparse.ArgumentParser()
    parser.add_argument('--name', required=True, help='-')
    args = parser.parse_args(argv)
    params = vars(args).copy()
    action = hello
    return action(**params)


if __name__ == '__main__':
    main()
