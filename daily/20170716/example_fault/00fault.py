import ctypes
libfault = ctypes.cdll.LoadLibrary("libfault.dylib")
libfault.disp(10)
libfault.disp(20)
