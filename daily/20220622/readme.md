## python

- changing to packaging guide using pyproject.toml

  - https://packaging.python.org/en/latest/tutorials/packaging-projects/
  - https://hatch.pypa.io/latest/intro/

### hatch

```console
$ pipx install hatch
$ hatch new "Hello World"
$ cd hello_world
$ hatch run pytest

# hatch build
```

### poetry

```console
$ pipx install poetry
$ poetry new --src byebye-world
$ cd byebye-world
$ poetry install
$ poetry run pytest

# poetry build
```
