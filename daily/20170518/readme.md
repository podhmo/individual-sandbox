# python build時のオプション見る

```python
import sysconfig
from pprint import pprint

pprint(sysconfig.get_config_vars())
```

# mac R macports Rのinstall

```console
$ sudo port info R
$ sudo port note R  # nothing
$ sudo port install R
```

最初rlangで検索して見てerlangが出て負けた。

## packageのinstall

```
$ r INSTALL CMD '<package name>'
```

もしくは

```
$ r
> install.packages("<package name>")
```
