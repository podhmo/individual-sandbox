def run(path: str):
    with open(path) as rf:
        text = rf.read()
    print(text.replace("\r", ""))


def main(argv=None):
    import argparse
    parser = argparse.ArgumentParser(description=None)
    parser.print_usage = parser.print_help
    parser.add_argument('path')
    args = parser.parse_args(argv)
    run(**vars(args))


if __name__ == '__main__':
    main()
