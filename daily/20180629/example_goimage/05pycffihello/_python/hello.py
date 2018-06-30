from cffi import FFI

ffi = FFI()

ffi.set_source(
    "pyhello",
    """
#include "hello.h"
""",
    extra_objects=["../hello.so"],
    include_dirs=["../"],
)

ffi.cdef("""
extern void Hello();
""")

if __name__ == "__main__":
    ffi.compile(verbose=True)
    from pyhello.lib import Hello
    Hello()
