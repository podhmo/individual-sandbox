python3.5

```bash
$ PYTHON=python3.5 make
ok
python3.5 bar.py
[1, 2, 3]
ng
python3.5 foo.py
Traceback (most recent call last):
  File "foo.py", line 5, in <module>
    bar()
  File ".//bar.py", line 2, in bar
    print(args)
NameError: name 'args' is not defined
make: *** [ng] Error 1
```

python2.7

```bash
$ PYTHON=python2.7 make
ok
python2.7 bar.py
[1, 2, 3]
ng
python2.7 foo.py
Traceback (most recent call last):
  File "foo.py", line 5, in <module>
    bar()
  File ".//bar.py", line 2, in bar
    print(args)
NameError: global name 'args' is not defined
make: *** [ng] Error 1
```
