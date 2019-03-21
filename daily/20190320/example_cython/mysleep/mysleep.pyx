from posix.unistd cimport sleep

def mysleep(x, unsigned int n):
    print("    ... start mysleep", x, n)
    sleep(n)
    print("    ... end mysleep", x)
    return x