import numpy as np


def reshape(a, shape):
    shape  # => (10, 2)
    return a.reshape(shape)


def main():
    a = np.arange(20)  # => multi-line..
# array([ 0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16,
#        17, 18, 19])
# ..multi-line
    reshape(a, (2, 10))  # => multi-line..
# array([[ 0,  1,  2,  3,  4,  5,  6,  7,  8,  9],
#        [10, 11, 12, 13, 14, 15, 16, 17, 18, 19]])
# ..multi-line
    reshape(a, (5, 4))  # => multi-line..
# array([[ 0,  1,  2,  3],
#        [ 4,  5,  6,  7],
#        [ 8,  9, 10, 11],
#        [12, 13, 14, 15],
#        [16, 17, 18, 19]])
# ..multi-line
    reshape(a, (4, 5))  # => multi-line..
# array([[ 0,  1,  2,  3,  4],
#        [ 5,  6,  7,  8,  9],
#        [10, 11, 12, 13, 14],
#        [15, 16, 17, 18, 19]])
# ..multi-line
    reshape(a, (10, 2))  # => multi-line..
# array([[ 0,  1],
#        [ 2,  3],
#        [ 4,  5],
#        [ 6,  7],
#        [ 8,  9],
#        [10, 11],
#        [12, 13],
#        [14, 15],
#        [16, 17],
#        [18, 19]])
# ..multi-line

main()
