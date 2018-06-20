from contextlib import ExitStack


def f0(io_or_filename):
    # DRY ?
    if isinstance(io_or_filename, str):
        with open(io_or_filename) as io:
            print("\tuse it")
            print("\t", io.read())
    else:
        io = io_or_filename
        print("\tuse it")
        print("\t", io.read())


def f1(io_or_filename):
    # hmm, naming is needed(function definition is needed)
    if isinstance(io_or_filename, str):
        with open(io_or_filename) as io:
            return f1(io)
    io = io_or_filename
    print("\tuse it")
    print("\t", io.read())


def f2(io_or_filename):
    with ExitStack() as s:
        io = io_or_filename
        if isinstance(io_or_filename, str):
            io = open(io_or_filename)
            s.callback(io.close)
        print("\tuse it")
        print("\t", io.read())


def main():
    import tempfile
    with tempfile.NamedTemporaryFile(mode="w") as wf:
        wf.write("hello")
        wf.flush()

        fs = [f0, f1, f2]
        for f in fs:
            print(f.__name__)
            f(wf.name)

        print("----------------------------------------")

        for f in fs:
            with open(wf.name) as rf:
                print(f.__name__)
                f(rf)


if __name__ == "__main__":
    main()
