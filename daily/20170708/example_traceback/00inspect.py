# http://qiita.com/ymko/items/b46d32b98f013f06d805
import inspect
import os


def location(depth=0):
    frame = inspect.currentframe(depth + 1)
    return os.path.basename(frame.f_code.co_filename), frame.f_code.co_name, frame.f_lineno


def func1():
    print(location())


def main():
    print(location())
    func1()


if __name__ == '__main__':
    main()
    exit(0)
