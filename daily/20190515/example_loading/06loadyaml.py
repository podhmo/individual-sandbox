import sys
import yaml
import pprint

if __name__ == "__main__":
    filename = sys.argv[1]
    with open(filename) as rf:
        loader = yaml.Loader(rf)

        def gen():
            while loader.check_data():
                yield loader.get_data()

        try:
            print(list(gen()))
        finally:
            pprint.pprint(vars(loader))
            loader.dispose()
