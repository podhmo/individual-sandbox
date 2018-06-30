from cffi import FFI


def main(filename):
    ffi = FFI()

    ffi.set_source(
        "pylibsize",
        """
        # include "libsize.h"
        """,
        extra_objects=["libsize.so"],
        include_dirs=["."],
    )

    ffi.cdef(
        """\
struct Size_return {
	int r0; /* w */
	int r1; /* h */
};

extern struct Size_return Size(char* p0);
        """
    )
    ffi.compile(verbose=True, debug=True)

    from pylibsize import lib, ffi
    sizeinfo = lib.Size(filename.encode("utf-8"))
    print(sizeinfo, sizeinfo.r0, sizeinfo.r1)


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--filename", default="../images/cat.png")
    args = parser.parse_args()
    main(args.filename)
