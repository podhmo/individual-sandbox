from dictknife import loading
from dictknife import LooseDictWalkingIterator


def run(src):
    d = loading.loadfile(src)
    for path, d in LooseDictWalkingIterator(["enum"]).iterate(d):
        print(path[-2])
        for x in d["enum"]:
            print("- {}".format(x))
        print("")

if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("src")
    args = parser.parse_args()
    run(args.src)
