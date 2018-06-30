import ctypes


class R(ctypes.Structure):
    __fields__ = [("r0", ctypes.c_char_p), ("r1", ctypes.c_int)]


myuuid = ctypes.cdll.LoadLibrary("./myuuid.so")
myuuid.Gen.restype = R


def tick():
    import os
    import subprocess
    subprocess.run(["ps", "u", f"{os.getpid()}"], check=True)


def main():
    for i in range(4):
        print(i)
        tick()
        run()
    tick()


def run():
    for i in range(1):
        print(myuuid.Gen().r0)


try:
    main = profile(main)
except Exception as e:
    print(e)

if __name__ == "__main__":
    main()
    print("ok")
