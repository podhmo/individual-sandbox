from cffi import FFI
ffi = FFI()

ffi.set_source(
    "_myuuid",
    """\
# include "myuuid.h"
    """,
    extra_objects=["myuuid.so"],
    include_dirs=["."],
)

ffi.cdef(
    """\
/* Return type for Gen */
struct Gen_return {
	char* r0;
	size_t r1;
};

// Gen :

extern struct Gen_return Gen();

// Free :

extern void Free(size_t p0);
"""
)


def tick():
    import os
    import subprocess
    subprocess.run(["ps", "u", f"{os.getpid()}"], check=True)


def main():
    ffi.compile(verbose=True, debug=True)
    tick()
    run()
    tick()
    run()
    tick()
    run()
    tick()
    run()
    tick()


def run():
    from _myuuid.lib import Gen, Free
    for i in range(100000):
        v = Gen()
        # print(ffi.string(v.r0).decode("utf-8"))
        Free(v.r1)


try:
    main = profile(main)
except Exception as e:
    print(e)

if __name__ == "__main__":
    main()
    print("ok")
