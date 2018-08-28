import traceback


def f():
    g()


def g():
    h()


def h():
    1 / 0


def main():
    try:
        f()
    except Exception as e:
        print(traceback.format_exc(limit=2))
        print("----------------------------------------")
        sframe = traceback.extract_tb(e.__traceback__, limit=2)[1]
        print(f"{sframe.filename}:{sframe.lineno}")


main()
