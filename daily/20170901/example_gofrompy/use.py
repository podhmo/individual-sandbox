from ctypes import cdll, c_longlong

lib = cdll.LoadLibrary("./fib.dylib")

lib.fib.argtypes = [c_longlong]
lib.fib.restype = c_longlong

print("fib(40) = {}".format(lib.fib(40)))
