```console
$ make 00
time python 00*.py
{"age": "20", "group_id": "1", "id": "1", "id_g": "1", "name": "foo", "name_g": "A"}
{"age": "20", "group_id": "1", "id": "2", "id_g": "1", "name": "bar", "name_g": "A"}
{"age": "10", "group_id": "2", "id": "3", "id_g": "2", "name": "boo", "name_g": "B"}

real	0m0.146s
user	0m0.077s
sys	0m0.025s
$ make 00
time python 00*.py
{"age": "20", "group_id": "1", "id": "1", "id_g": "1", "name": "foo", "name_g": "A"}
{"age": "20", "group_id": "1", "id": "2", "id_g": "1", "name": "bar", "name_g": "A"}
{"age": "10", "group_id": "2", "id": "3", "id_g": "2", "name": "boo", "name_g": "B"}

real	0m0.127s
user	0m0.086s
sys	0m0.022s
$ make 00
time python 00*.py
{"age": "20", "group_id": "1", "id": "1", "id_g": "1", "name": "foo", "name_g": "A"}
{"age": "20", "group_id": "1", "id": "2", "id_g": "1", "name": "bar", "name_g": "A"}
{"age": "10", "group_id": "2", "id": "3", "id_g": "2", "name": "boo", "name_g": "B"}

real	0m0.110s
user	0m0.079s
sys	0m0.020s
$ make 01
time python 01*.py
   id name  age  group_id  id_g name_g
0   1  foo   20         1     1      A
1   2  bar   20         1     1      A
2   3  boo   10         2     2      B

real	0m1.438s
user	0m1.011s
sys	0m0.177s
$ make 01
time python 01*.py
   id name  age  group_id  id_g name_g
0   1  foo   20         1     1      A
1   2  bar   20         1     1      A
2   3  boo   10         2     2      B

real	0m1.348s
user	0m1.147s
sys	0m0.137s
$ make 01
time python 01*.py
   id name  age  group_id  id_g name_g
0   1  foo   20         1     1      A
1   2  bar   20         1     1      A
2   3  boo   10         2     2      B

real	0m1.365s
user	0m1.100s
sys	0m0.131s
```
