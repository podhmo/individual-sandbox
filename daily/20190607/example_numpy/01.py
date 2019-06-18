import numpy as np

# 3. Create a null vector of size 10 (★☆☆)
Z = np.zeros(10)
Z  # => array([0., 0., 0., 0., 0., 0., 0., 0., 0., 0.])

# 4.  How to find the memory size of any array (★☆☆)
Z = np.zeros((10, 10))
f"{Z.size * Z.itemsize} bytes"  # => '800 bytes'

# 6.  Create a null vector of size 10 but the fifth value which is 1 (★☆☆)

Z = np.zeros(10)
Z[4] = 1
Z  # => array([0., 0., 0., 0., 1., 0., 0., 0., 0., 0.])

# 7.  Create a vector with values ranging from 10 to 49 (★☆☆)
Z = np.arange(10, 50)
Z  # => multi-line..
# array([10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26,
#        27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43,
#        44, 45, 46, 47, 48, 49])
# ..multi-line

# 8.  Reverse a vector (first element becomes last) (★☆☆)
Z = np.arange(50)
Z = Z[::-1]
Z  # => multi-line..
# array([49, 48, 47, 46, 45, 44, 43, 42, 41, 40, 39, 38, 37, 36, 35, 34, 33,
#        32, 31, 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 17, 16,
#        15, 14, 13, 12, 11, 10,  9,  8,  7,  6,  5,  4,  3,  2,  1,  0])
# ..multi-line

# 9.  Create a 3x3 matrix with values ranging from 0 to 8 (★☆☆)
Z = np.arange(9).reshape(3, 3)
Z  # => multi-line..
# array([[0, 1, 2],
#        [3, 4, 5],
#        [6, 7, 8]])
# ..multi-line

# 10. Find indices of non-zero elements from \[1,2,0,0,4,0\] (★☆☆)
nz = np.nonzero([1, 2, 0, 0, 4, 0])
nz  # => (array([0, 1, 4]),)

# 11. Create a 3x3 identity matrix (★☆☆)
Z = np.eye(3)
Z  # => multi-line..
# array([[1., 0., 0.],
#        [0., 1., 0.],
#        [0., 0., 1.]])
# ..multi-line

# 12. Create a 3x3x3 array with random values (★☆☆)
Z = np.random.random((3, 3, 3))
Z  # => multi-line..
# array([[[0.63601781, 0.07777646, 0.14073474],
#         [0.60377164, 0.96157653, 0.40408691],
#         [0.48071344, 0.84361189, 0.02911445]],
# 
#        [[0.78867224, 0.55297647, 0.41425805],
#         [0.52057293, 0.37516557, 0.5885932 ],
#         [0.80901401, 0.34400083, 0.54899279]],
# 
#        [[0.95995213, 0.82366224, 0.76095345],
#         [0.03714615, 0.76912232, 0.85881491],
#         [0.68841777, 0.96701755, 0.48036572]]])
# ..multi-line

# 13. Create a 10x10 array with random values and find the minimum and maximum values (★☆☆)

Z = np.random.random((10, 10))
Z.min(), Z.max()  # => (0.0016360080062399751, 0.9825844926764734)

# 14. Create a random vector of size 30 and find the mean value (★☆☆)

Z = np.random.random(30)
m = Z.mean()
m  # => 0.5119997260006314

# 15. Create a 2d array with 1 on the border and 0 inside (★☆☆)
Z = np.ones((10, 10))
Z[1:-1, 1:-1] = 0
Z  # => multi-line..
# array([[1., 1., 1., 1., 1., 1., 1., 1., 1., 1.],
#        [1., 0., 0., 0., 0., 0., 0., 0., 0., 1.],
#        [1., 0., 0., 0., 0., 0., 0., 0., 0., 1.],
#        [1., 0., 0., 0., 0., 0., 0., 0., 0., 1.],
#        [1., 0., 0., 0., 0., 0., 0., 0., 0., 1.],
#        [1., 0., 0., 0., 0., 0., 0., 0., 0., 1.],
#        [1., 0., 0., 0., 0., 0., 0., 0., 0., 1.],
#        [1., 0., 0., 0., 0., 0., 0., 0., 0., 1.],
#        [1., 0., 0., 0., 0., 0., 0., 0., 0., 1.],
#        [1., 1., 1., 1., 1., 1., 1., 1., 1., 1.]])
# ..multi-line

# 16. How to add a border (filled with 0's) around an existing array? (★☆☆)

Z = np.ones((5, 5))
Z = np.pad(Z, pad_width=1, mode="constant", constant_values=0)

# 17. What is the result of the following expression? (★☆☆)

0 * np.nan  # => nan
np.nan == np.nan  # => False
np.inf > np.nan  # => False
np.nan - np.nan  # => nan
np.nan in set([np.nan])  # => True
0.3 == 3 * 0.1  # => False

# 18. Create a 5x5 matrix with values 1,2,3,4 just below the diagonal (★☆☆)

Z = np.diag(1 + np.arange(4), k=-1)
Z  # => multi-line..
# array([[0, 0, 0, 0, 0],
#        [1, 0, 0, 0, 0],
#        [0, 2, 0, 0, 0],
#        [0, 0, 3, 0, 0],
#        [0, 0, 0, 4, 0]])
# ..multi-line



