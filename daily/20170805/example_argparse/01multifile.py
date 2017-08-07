import sys
import argparse


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("files", nargs="*", type=argparse.FileType("r"), default=sys.stdin)
    args = parser.parse_args()
    print(args)


main()
