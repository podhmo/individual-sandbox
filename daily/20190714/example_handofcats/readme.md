```console
$ python xxx.py
Traceback (most recent call last):
  File "xxx.py", line 4, in <module>
    @as_command
  File "VENV/handofcats/handofcats/__init__.py", line 30, in as_command
    return call(fn, level=level, argv=argv)
  File "VENV/handofcats/handofcats/__init__.py", line 25, in call
    return driver.run(argv)
  File "VENV/handofcats/handofcats/driver.py", line 99, in run
    self.setup_parser(parser)
  File "VENV/handofcats/handofcats/driver.py", line 85, in setup_parser
    self._setup_type(opt, kwargs)
  File "VENV/handofcats/handofcats/driver.py", line 69, in _setup_type
    elif issubclass(opt.type, (list, tuple)):
TypeError: issubclass() arg 1 must be a class
```
