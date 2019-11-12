

def hello(name: str) -> None:
    print(f"hello {name}")

def main(argv=None):
    import argparse
    parser = argparse.ArgumentParser(description=None)
    parser.print_usage = parser.print_help
    parser.add_argument('name')
    args = parser.parse_args(argv)
    hello(**vars(args))


if __name__ == '__main__':
    main()
