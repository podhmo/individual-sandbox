動きを試してみるとき

```console
$ make one
python fetch.py -c config.json  -x 1 -y 2
id,x,y,x_y_rate
1,1,2,0.5

$ make many
python fetch.py -c config.json -x 1 -x 2 -x 3 -x 4 -x 5 -y 2 -y 1 -y 0 -y 1 -y 2
id,x,y,x_y_rate
1,1,2,0.5
2,4,1,4.0
3,9,0,-
4,16,1,16.0
5,25,2,12.5

$ SIZE=3 make
python fetch.py -c config.json --size=3 --input ./test-input.csv | tee dist/test-output.csv
id,x,y,x_y_rate
1,1,-50,-0.02
2,4,-49,-0.08163265306122448
3,9,-48,-0.1875
```

全部実行する時

```console
# どういうコマンドが実行されるかは -n 付きで実行
$ make -n
mkdir -p dist
python fetch.py -c config.json  --input ./test-input.csv | tee dist/test-output.csv

# 全部実行
$ PREFIX=test make
id,x,y,x_y_rate
1,1,-50,-0.02
2,4,-49,-0.08163265306122448
3,9,-48,-0.1875
4,16,-47,-0.3404255319148936
5,25,-46,-0.5434782608695652
...
40,1600,-11,-145.45454545454547
41,1681,-10,-168.1
42,1764,-9,-196.0
43,1849,-8,-231.125
ERROR

# resume
$ RESUME=1 PREFIX=test make
44,1936,-7,-276.57142857142856
45,2025,-6,-337.5
46,2116,-5,-423.2
47,2209,-4,-552.25
48,2304,-3,-768.0
49,2401,-2,-1200.5
50,2500,-1,-2500.0
...
96,9216,45,204.8
97,9409,46,204.54347826086956
98,9604,47,204.3404255319149
99,9801,48,204.1875
100,10000,49,204.08163265306123
101,10201,50,204.02
```
