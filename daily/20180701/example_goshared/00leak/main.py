import ctypes

myuuid = ctypes.cdll.LoadLibrary("./myuuid.so")
myuuid.Gen.restype = ctypes.c_char_p


def tick():
    import os
    import subprocess
    subprocess.run(["ps", "u", f"{os.getpid()}"], check=True)


def main():
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
    for i in range(100000):
        myuuid.Gen()


try:
    main = profile(main)
except Exception as e:
    print(e)

if __name__ == "__main__":
    main()
    print("ok")
