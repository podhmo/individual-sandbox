from cpython cimport array


def disp(int n):
    cdef array.array a = array.array('i', [1, 2, 3])
    return a[n]