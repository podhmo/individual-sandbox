import traceback


def func1():
    traceback.print_stack(limit=1)


def main():
    traceback.print_stack(limit=1)
    func1()


if __name__ == '__main__':
    main()
    exit(0)
