def hello(out=None):
    print("hello", file=out)


def main():
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("-f", "--file", type=argparse.FileType("w"))
    args = parser.parse_args()
    hello(out=args.file)


if __name__ == "__main__":
    main()
