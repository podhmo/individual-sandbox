from dictknife import loading


def dig(d):
    print(d)


def run(src):
    d = loading.loadfile(src)
    target = d["module"]["model"]
    dig(target)


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("src", default=None)
    args = parser.parse_args()

    run(args.src)
