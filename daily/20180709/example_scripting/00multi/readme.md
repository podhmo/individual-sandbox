```console
time (python pp.py 00.json && python pp.py 01.json && python pp.py 02.json)
foo(age20)
bar(age21)
boo(age20)

real	0m0.367s
user	0m0.321s
sys	0m0.037s
```


```console
time (python multi.py pp.py 00.json -- pp.py 01.json -- pp.py 02.json)
foo(age20)
bar(age21)
boo(age20)
end

real	0m0.141s
user	0m0.122s
sys	0m0.018s
```
