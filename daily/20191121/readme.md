## kamidna jinja2 python tbの位置がおかしいものを調べる

- kamidanaのエラー表示が壊れた
- 原因はpython3.8の変更後
- 後に、ファイルは正しいが行数が正しくないという状況
- 具体的にはテンプレートから生成されるpythonコードの行数が表示される様になってしまっている

### pythonコードの確認

個別に見る場合はこう。

```python
env = Environment()
source = "<source code>"
ast = env._parse(source, name, filename)
code = env._generate(ast, name, filename)
```


一度に見たい場合はこう

```python
import os.path
from jinja2 import Environment, FileSystemLoader


here = os.path.dirname(__file__)
env = Environment(loader=FileSystemLoader("."))
env.compile_templates(here, zip=None, log_function=print)
```

### 例外部分は

例外のハンドリングはenvironmentの部分

```python

    def render(self, *args, **kwargs):
        """This method accepts the same arguments as the `dict` constructor:
        A dict, a dict subclass or some keyword arguments.  If no arguments
        are given the context will be empty.  These two calls do the same::

            template.render(knights='that say nih')
            template.render({'knights': 'that say nih'})

        This will return the rendered template as unicode string.
        """
        vars = dict(*args, **kwargs)
        try:
            return concat(self.root_render_func(self.new_context(vars)))
        except Exception:
            exc_info = sys.exc_info()
        return self.environment.handle_exception(exc_info, True)
```

ここでhandle_exceptionは

```python
    def handle_exception(self, exc_info=None, rendered=False, source_hint=None):
        """Exception handling helper.  This is used internally to either raise
        rewritten exceptions or return a rendered traceback for the template.
        """
        global _make_traceback
        if exc_info is None:
            exc_info = sys.exc_info()

        # the debugging module is imported when it's used for the first time.
        # we're doing a lot of stuff there and for applications that do not
        # get any exceptions in template rendering there is no need to load
        # all of that.
        if _make_traceback is None:
            from jinja2.debug import make_traceback as _make_traceback
        traceback = _make_traceback(exc_info, source_hint)
        if rendered and self.exception_formatter is not None:
            return self.exception_formatter(traceback)
        if self.exception_handler is not None:
            self.exception_handler(traceback)
        exc_type, exc_value, tb = traceback.standard_exc_info
        reraise(exc_type, exc_value, tb)
```

つまり使われるのは `jinja2.debug:make_traceback()`
