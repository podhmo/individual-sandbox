import ctypes


class Size_return(ctypes.Structure):
    __fields__ = [("r0", ctypes.c_int), ("r1", ctypes.c_int)]


def main(filename):
    libsize = ctypes.cdll.LoadLibrary("./libsize.so")

    print(libsize.Size)
    libsize.Size.argtypes = [
        ctypes.c_char_p,
    ]
    libsize.Size.restype = Size_return
    cfname = filename.encode("utf-8")
    r = libsize.Size(cfname)
    print(r)


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--filename", default="../images/cat.png")
    args = parser.parse_args()
    main(args.filename)
