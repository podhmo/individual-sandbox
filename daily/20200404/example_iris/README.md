```console
$ make 00 01 02
time r CMD BATCH --vanilla --slave 00iris.r 00iris.r.out
        0.54 real         0.17 user         0.12 sys
time python 01iris.py  | tee 01iris.py.out
       sepal length (cm)  sepal width (cm)  petal length (cm)  petal width (cm)
count         150.000000        150.000000         150.000000        150.000000
mean            5.843333          3.057333           3.758000          1.199333
std             0.828066          0.435866           1.765298          0.762238
min             4.300000          2.000000           1.000000          0.100000
25%             5.100000          2.800000           1.600000          0.300000
50%             5.800000          3.000000           4.350000          1.300000
75%             6.400000          3.300000           5.100000          1.800000
max             7.900000          4.400000           6.900000          2.500000

real	0m2.241s
user	0m1.027s
sys	0m0.326s
time python 02iris.py  | tee 02iris.py.out
       petalLength  petalWidth  sepalLength  sepalWidth
count   150.000000  150.000000   150.000000  150.000000
mean      3.758000    1.199333     5.843333    3.057333
std       1.765298    0.762238     0.828066    0.435866
min       1.000000    0.100000     4.300000    2.000000
25%       1.600000    0.300000     5.100000    2.800000
50%       4.350000    1.300000     5.800000    3.000000
75%       5.100000    1.800000     6.400000    3.300000
max       6.900000    2.500000     7.900000    4.400000

real	0m0.748s
user	0m0.573s
sys	0m0.168s
```
