from posix.unistd cimport sleep

def mysleep(x, unsigned int n):
    print("    ... start mysleep", x, n)
    with nogil:
        sleep(n)
    print("    ... end mysleep", x)
    return x