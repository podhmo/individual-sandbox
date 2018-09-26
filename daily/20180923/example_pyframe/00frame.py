import inspect


def main():
    f = inspect.currentframe()
    co = f.f_code
    print(co.co_filename, f.f_lineno, co.co_name)


main()
