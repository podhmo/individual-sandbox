"""
以下などのようにして実行

$ python <file>.py 10 20
"""
import sys


def main(argv=None, out=None):
    argv = argv or sys.argv
    x = argv[1]
    y = argv[2]
    print(f"{x} + {y} = {int(x) + int(y)} です")


if __name__ == "__main__":
    main()
