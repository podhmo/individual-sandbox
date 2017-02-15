def run(target):
    print("hello {target}".format(target=target))


def main():
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--target", default="world")
    args = parser.parse_args()
    run(args.target)


if __name__ == "__main__":
    main()
