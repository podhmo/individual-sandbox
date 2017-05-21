# python mypy弄りたくなった

この辺が参考になる

- http://mypy.readthedocs.io/en/latest/cheat_sheet_py3.html

とりあえず以下の様にしている。 `.mypy_cache` と言うものが作られるっぽい。

```console
$ mypy -v --strict main.py
```

typeshedの中身自体はmypyがコピーを持っているっぽい。

```lisp
(add-to-list 'auto-mode-alist '("\\.pyi$" . python-mode))
```

## protocol

protocolって実装されたのだっけ？とりあえずUnionで凌ごう。

```python
from typing import Uninon
import logging


logger : Union[loging.Logger, logging.LoggerAdapter]
```


