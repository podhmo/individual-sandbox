```console
$ python main.py | tee output.csv
start 1
{"id": "1", "x": "10", "y": "20"}
start 2
^CTraceback (most recent call last):
  File "main.py", line 12, in <module>
    time.sleep(2)
KeyboardInterrupt
make: *** [default] Error 130

$ RESUME=1 python main.py | tee output.csv
start 2
{"id": "2", "x": "100", "y": "100"}
```
