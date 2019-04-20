## graphql postgresql

- https://hub.docker.com/r/graphile/postgraphile/
- https://medium.com/coderbunker/a-better-way-to-develop-your-graphql-api-using-docker-postgresql-postgraphile-7a1ae034b826

もしくは

- https://github.com/hasura/graphql-engine

##
python orator

orator意外と結構大変だなー。

## python IntFlag

```
  Complete output from command VENV/bin/python VENV/lib/python3.7/site-packages/pip install --ignore-installed --no-user --prefix /tmp/pip-build-env-8tyudvd3/overlay --no-warn-script-location --no-binary :none: --only-binary :none: -i https://pypi.org/simple -- setuptools>=40.8.0 wheel:
  Traceback (most recent call last):
    File "/usr/lib/python3.7/runpy.py", line 193, in _run_module_as_main
      "__main__", mod_spec)
    File "/usr/lib/python3.7/runpy.py", line 85, in _run_code
      exec(code, run_globals)
    File "VENV/lib/python3.7/site-packages/pip/__main__.py", line 16, in <module>
      from pip._internal import main as _main  # isort:skip # noqa
    File "VENV/lib/python3.7/site-packages/pip/_internal/__init__.py", line 4, in <module>
      import locale
    File "/usr/lib/python3.7/locale.py", line 16, in <module>
      import re
    File "/usr/lib/python3.7/re.py", line 143, in <module>
      class RegexFlag(enum.IntFlag):
  AttributeError: module 'enum' has no attribute 'IntFlag'
```

enum34をuninstallすれば良い。

- https://github.com/pyinstaller/pyinstaller/issues/3146#issuecomment-354913915
