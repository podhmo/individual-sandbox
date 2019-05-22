import sys
import yaml

# load
# - scan
# - parse
# - compose


if __name__ == "__main__":
    filename = sys.argv[1]
    with open(filename) as rf:
        for ev in yaml.scan(rf):
            print(":", ev, type(ev))
        print("----------------------------------------")
    with open(filename) as rf:
        for ev in yaml.parse(rf):
            print("::", ev, type(ev), ev.start_mark)
        print("----------------------------------------")
    with open(filename) as rf:
        for ev in yaml.load(rf):
            print("::", ev)
