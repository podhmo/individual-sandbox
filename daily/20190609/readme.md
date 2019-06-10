## jinja2 jinja2に組み込まれているloaderを整理する

```console
$ python -c 'import jinja2.loaders as l; import inspect as i; print("\n".join([v.__qualname__ for k,v in l.__dict__.items() if i.isclass(v) and issubclass(v, l.BaseLoader)]))'
BaseLoader
FileSystemLoader
PackageLoader
DictLoader
FunctionLoader
PrefixLoader
ChoiceLoader
ModuleLoader
```

以下の違いってなんだろう？

- PackageLoader
- ModuleLoader

### misc

- [list_templates()](http://jinja.pocoo.org/docs/2.10/api/#jinja2.Environment.list_templates)


## jinja2 extensionsの使われ方を整理する

- 定義されているのは jinja2.ext
- 有効にする方法はjinja2.environment.Environmentのextensions引数

jinja2/environment.py

```python
def load_extensions(environment, extensions):
    """Load the extensions from the list and bind it to the environment.
    Returns a dict of instantiated environments.
    """
    result = {}
    for extension in extensions:
        if isinstance(extension, string_types):
            extension = import_string(extension)
        result[extension.identifier] = extension(environment)
    return result
```

ここの `import_string()` でみつけたfactoryを呼ぶだけか。

http://jinja.pocoo.org/docs/2.10/extensions/



