
## memo

## 今のyapfの実装の問題

以下の様な形式のimportを壊さずreformatしてくれない

```python
from foo import (
    x,
    y,
    z,
)
from foo import (
    x,
)
from foo import (  # NOQA
    x,
    y,
)
```

monkey patchした yyapf.py ではどうにかなる。

```bash
yapf src.py
python bin/yyapf.py src.py
```

## 設定の意味

style.yapf

```
[style]
based_on_style = pep8
# column_limit = 160
spaces_around_power_operator = true
dedent_closing_brackets=true
split_penalty_import_names = 100000
split_arguments_when_comma_terminated = true
join_multiple_lines = false
```


### spaces_around_power_operator

```python
x ** 8

# reformat to(false)
x**8

# reformat to(true)
x ** 8
```

### dedent_closing_brackets

```python
from x import (
    y,
    z,
)

# reformat to(false)
from x import (
    y,
    z, )

# reformat to(true)
from x import (
    y,
    z,
)
```

### split_arguments_when_comma_terminated

```python
foo(1, 2, 3)

# reformat to(true)
foo(1, 2, 3)
```

```python
foo(1, 2, 3,)

# reformat to(true)
foo(
    1,
    2,
    3,
)
```

### join_multiple_lines

```python
if foo(bar, baz): continue

# reformat(false)
if foo(bar, baz):
    continue

# reformat(true)
if foo(bar, baz): continue
```

```python
if foo(bar, baz):
    continue

# reformat(false)
if foo(bar, baz):
    continue

# reformat(true)
if foo(bar, baz):
    continue
```

