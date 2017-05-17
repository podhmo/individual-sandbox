`-j` 付けた場合

```console
$ make -j 7
make -f echo.mk
sleep 1
sleep 1
sleep 1
sleep 1
sleep 1
sleep 1
sleep 1
echo 1
echo 4
echo 5
1
4
echo 6
echo 3
5
echo 2
6
echo 0
3
sleep 1
2
sleep 1
0
sleep 1
sleep 1
sleep 1
sleep 1
sleep 1
echo 7
echo 9
7
echo 12
echo 11
9
12
echo 13
11
echo 10
echo 8
13
sleep 1
10
8
echo 14
14
```

`-j` 付けない場合

```console
$ make
make -f echo.mk
sleep 1
echo 0
0
sleep 1
echo 1
1
sleep 1
echo 2
2
sleep 1
echo 3
3
sleep 1
echo 4
4
sleep 1
echo 5
5
sleep 1
echo 6
6
sleep 1
echo 7
7
sleep 1
echo 8
8
sleep 1
echo 9
9
sleep 1
echo 10
10
sleep 1
echo 11
11
sleep 1
echo 12
12
sleep 1
echo 13
13
sleep 1
echo 14
14
```
