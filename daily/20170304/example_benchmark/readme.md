https://github.com/voidfiles/python-serialization-benchmark

## py3 support

- print syntax
- xrange

## benchmark normal

```
(schema) [14:29:52] ~/vboxshare/venvs/schema/python-serialization-benchmark $ python benchmark.py
Library                  Many Objects    One Object
---------------------  --------------  ------------
Custom                      0.0237799     0.0135982
serpy                       0.0999041     0.047271
Strainer                    0.105224      0.049396
Marshmallow                 1.30832       0.650571
Lollipop                    1.84514       0.865072
Django REST Framework       1.8821        1.1738
(schema) [14:30:09] ~/vboxshare/venvs/schema/python-serialization-benchmark $ python benchmark.py
Library                  Many Objects    One Object
---------------------  --------------  ------------
Custom                      0.0267601     0.0104561
serpy                       0.0801232     0.0452709
Strainer                    0.093446      0.046381
Marshmallow                 1.28133       0.822676
Lollipop                    1.87705       0.912775
Django REST Framework       1.68549       1.14579
(schema) [14:30:20] ~/vboxshare/venvs/schema/python-serialization-benchmark $ python benchmark.py
Library                  Many Objects    One Object
---------------------  --------------  ------------
Custom                       0.022717     0.0114
serpy                        0.080195     0.0412009
Strainer                     0.09674      0.050848
Marshmallow                  1.28368      0.65332
Lollipop                     1.81572      0.858437
Django REST Framework        1.72237      1.09924
```

## update_fields=False

```
(schema) [14:33:01] ~/vboxshare/venvs/schema/python-serialization-benchmark $ python benchmark.py
Library                  Many Objects    One Object
---------------------  --------------  ------------
Custom                      0.023118      0.013799
serpy                       0.092134      0.0434709
Strainer                    0.0987539     0.0467339
Marshmallow                 1.24925       0.635535
Lollipop                    1.79049       0.886192
Django REST Framework       1.80183       1.13642
(schema) [14:33:26] ~/vboxshare/venvs/schema/python-serialization-benchmark $ python benchmark.py
Library                  Many Objects    One Object
---------------------  --------------  ------------
Custom                      0.0257061     0.0114901
serpy                       0.0776699     0.045084
Strainer                    0.0964749     0.0442262
Marshmallow                 1.20168       0.619551
Lollipop                    1.95295       0.887621
Django REST Framework       1.88507       1.37347
(schema) [14:33:36] ~/vboxshare/venvs/schema/python-serialization-benchmark $ python benchmark.py
Library                  Many Objects    One Object
---------------------  --------------  ------------
Custom                      0.025732      0.010673
serpy                       0.085058      0.0445771
Strainer                    0.0905201     0.046479
Marshmallow                 1.26245       0.653202
Lollipop                    1.84938       1.04788
Django REST Framework       1.66696       1.35414
```
