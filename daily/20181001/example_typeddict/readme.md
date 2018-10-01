00

```console
$ mypy --strict 00*.py
00overwrite.py:10: error: Cannot overwrite TypedDict field "name" while extending
00overwrite.py:16: error: Incompatible types (expression has type "str", TypedDict item "age" has type "int")
```

01

```console
$ mypy --strict 01*.py
mypy --strict 01*.py
01access.py:12: error: TypedDict "PersonOptional" has no key 'ame'
    01access.py:13: error: TypedDict "PersonOptional" has no key 'ame'
```


