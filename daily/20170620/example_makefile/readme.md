00

```
$ time make 00 -j 10
time make -f 00makefile
for i in src/*; do echo $i; cat $i; sleep 1; done
src/item1.txt
1
src/item10.txt
10
src/item11.txt
11
src/item12.txt
12
src/item13.txt
13
src/item14.txt
14
src/item15.txt
15
src/item16.txt
16
src/item17.txt
17
src/item18.txt
18
src/item19.txt
19
src/item2.txt
2
src/item20.txt
20
src/item3.txt
3
src/item4.txt
4
src/item5.txt
5
src/item6.txt
6
src/item7.txt
7
src/item8.txt
8
src/item9.txt
9
       20.25 real         0.04 user         0.07 sys
```

01
```
$ make 01 -j 10
time make -f 01makefile
echo src/item1.txt; cat src/item1.txt; sleep 1;
echo src/item10.txt; cat src/item10.txt; sleep 1;
echo src/item11.txt; cat src/item11.txt; sleep 1;
echo src/item12.txt; cat src/item12.txt; sleep 1;
src/item1.txt
echo src/item13.txt; cat src/item13.txt; sleep 1;
echo src/item14.txt; cat src/item14.txt; sleep 1;
src/item10.txt
echo src/item15.txt; cat src/item15.txt; sleep 1;
src/item11.txt
1
src/item12.txt
echo src/item16.txt; cat src/item16.txt; sleep 1;
src/item13.txt
10
echo src/item17.txt; cat src/item17.txt; sleep 1;
11
src/item14.txt
12
echo src/item18.txt; cat src/item18.txt; sleep 1;
src/item15.txt
13
src/item16.txt
14
src/item17.txt
15
src/item18.txt
16
17
18
echo src/item19.txt; cat src/item19.txt; sleep 1;
src/item19.txt
19
echo src/item2.txt; cat src/item2.txt; sleep 1;
echo src/item20.txt; cat src/item20.txt; sleep 1;
echo src/item3.txt; cat src/item3.txt; sleep 1;
src/item2.txt
src/item20.txt
src/item3.txt
2
echo src/item4.txt; cat src/item4.txt; sleep 1;
20
3
echo src/item5.txt; cat src/item5.txt; sleep 1;
src/item4.txt
echo src/item6.txt; cat src/item6.txt; sleep 1;
echo src/item7.txt; cat src/item7.txt; sleep 1;
echo src/item8.txt; cat src/item8.txt; sleep 1;
4
src/item5.txt
src/item6.txt
echo src/item9.txt; cat src/item9.txt; sleep 1;
src/item7.txt
5
6
src/item8.txt
7
src/item9.txt
8
9
        2.12 real         0.08 user         0.13 sys
```
