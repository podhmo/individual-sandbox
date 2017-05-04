```bash
$ make
time zenmai hello.json
hello: world
        1.28 real         0.78 user         0.19 sys
time python -m zenmai hello.json
hello: world
        0.29 real         0.23 user         0.04 sys
time zenmai hello.json
hello: world
        0.75 real         0.63 user         0.10 sys
time python -m zenmai hello.json
hello: world
        0.26 real         0.21 user         0.04 sys
```
