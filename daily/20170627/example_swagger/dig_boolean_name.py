from dictknife import loading
from dictknife import LooseDictWalkingIterator


def run(src):
    d = loading.loadfile(src)
    for path, d in LooseDictWalkingIterator(["type"]).iterate(d):
        if d["type"] == "boolean":
            if "name" in d:
                name = d["name"]
            else:
                name = path[-2]
            print(name, d.get("description"))


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("src")
    args = parser.parse_args()
    run(args.src)
