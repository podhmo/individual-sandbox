import ctypes


def main(filename):
    libsize = ctypes.cdll.LoadLibrary("./libsize.so")

    print(libsize.Size)
    libsize.Size.argtypes = [
        ctypes.c_char_p,
        ctypes.POINTER(ctypes.c_int),
        ctypes.POINTER(ctypes.c_int)
    ]
    libsize.Size.restype = ctypes.c_int
    w = ctypes.c_int(0)
    h = ctypes.c_int(0)
    r = libsize.Size(filename.encode("utf-8"), ctypes.pointer(w), ctypes.pointer(h))
    print(r, w.value, h.value)


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--filename", default="../images/cat.png")
    args = parser.parse_args()
    main(args.filename)
