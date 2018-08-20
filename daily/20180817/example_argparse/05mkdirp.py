import argparse
import os.path


def hello(out=None):
    print("hello", file=out)


class MyFileType(argparse.FileType):
    def __call__(self, string):
        if "w" in self._mode:
            os.makedirs(os.path.dirname(string), exist_ok=True)
        return super().__call__(string)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("-f", "--file", type=MyFileType("w"))
    args = parser.parse_args()
    hello(out=args.file)


if __name__ == "__main__":
    main()
