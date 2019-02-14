## python jinja2 kamidana

- templateのpathの解決方法を変えたい
- 利用するtemplateの位置を尊重したい

### jinja2のinternalcode

inernalcodeってどういうやつだっけ？

```
def internalcode(f):
    """Marks the function as internally used"""
    internal_code.add(f.__code__)
    return f
```

後で調べる。ふつうに内部で使われるためのannotation的なものだけれど。

### pathの解決方法

少なくとも`{% extends "<path>"%}` を呼び出すタイミングの自分自身の情報が取れないとダメ。

わかったのはEnvironmentのget_templateの時点ではparentが手に入るという点。ただtracebackが取れるのは`_load_template()`から。

```
@jinja2 <kamidana.driver.MyEnvironment object at 0x7f49b725ee48> base/base.html.j2
...
  File "jinja2/asyncsupport.py", line 76, in render
    return original_render(self, *args, **kwargs)
  File "jinja2/environment.py", line 1005, in render
    return concat(self.root_render_func(self.new_context(vars)))
  File "<template>", line 11, in root
  File "jinja2/environment.py", line 830, in get_template
    return self._load_template(name, self.make_globals(globals))
  File "jinja2/environment.py", line 804, in _load_template
    template = self.loader.load(self, name, globals)
  File "jinja2/loaders.py", line 113, in load
    source, filename, uptodate = self.get_source(environment, name)
  File "kamidana/driver.py", line 27, in get_source
    traceback.print_stack()
@ base/base.html.j2
```

#### 追記

Environmentの以下のメソッドの関係を把握しきれていないかも?

- (`_load_template()`)
- `select_template()`
- `get_or_select_template()`
- `get_template()`
- (`from_string()`)
- (`render()`)

hmm

```console
$ pyinspect inspect jinja2.environment:Environment
jinja2.environment:Environment <- builtins:object
...
    [method] get_or_select_template(self, template_name_or_list, parent=None, globals=None)
        [method] select_template(self, names, parent=None, globals=None)
            [method] make_globals(self, d)
            [method] join_path(self, template, parent)
            [method] _load_template(self, name, globals)
        [method] get_template(self, name, parent=None, globals=None)
            [method] _load_template(self, name, globals)
            [method] join_path(self, template, parent)
            [method] make_globals(self, d)
```

とりあえずやるならget_templateとselect_templateを拡張しないとという感じそう？

意味的にはめちゃくちゃ単純でselect_templateはO(N)の探索が入るだけ。

#### 追記

とりあえずget_template()しかextendsのときには呼ばれないな。

#### 追記

まじめにコードのなかを見てみるとincludeの時点で分岐がある。extendsの方はget_template一択

compiler.py
```python
class CodeGenerator(NodeVisitor):
    def visit_Include(self, node, frame):
        """Handles includes."""
        if node.ignore_missing:
            self.writeline('try:')
            self.indent()

        func_name = 'get_or_select_template'
        if isinstance(node.template, nodes.Const):
            if isinstance(node.template.value, string_types):
                func_name = 'get_template'
            elif isinstance(node.template.value, (tuple, list)):
                func_name = 'select_template'
        elif isinstance(node.template, (nodes.Tuple, nodes.List)):
            func_name = 'select_template'
...

    def visit_Extends(self, node, frame):
        """Calls the extender."""

...

        self.writeline('parent_template = environment.get_template(', node)
        self.visit(node.template, frame)
...

```

### get_template()

もう少しまじめにコードを読むとparentが含まれていた時に勝手にjoinされてしまう？

```python
    @internalcode
    def get_template(self, name, parent=None, globals=None):
        if isinstance(name, Template):
            return name
        if parent is not None:
            name = self.join_path(name, parent)
        return self._load_template(name, self.make_globals(globals))
```

join_pathの定義変えられるかも？さすがじゃん？？

```python
    def join_path(self, template, parent):
        """Join a template with the parent.  By default all the lookups are
        relative to the loader root so this method returns the `template`
        parameter unchanged, but if the paths should be relative to the
        parent template, this function can be used to calculate the real
        template name.

        Subclasses may override this method and implement template path
        joining here.
        """
        return template
```

### TemplateNotFound

単にNoneを返せばTemplateNotFoundエラーを返してくれるかも？(今自前で頑張っているところがある)

```python
class FunctionLoader(BaseLoader):
    def __init__(self, load_func):
        self.load_func = load_func

    def get_source(self, environment, template):
        rv = self.load_func(template)
        if rv is None:
            raise TemplateNotFound(template)
        elif isinstance(rv, string_types):
            return rv, None, None
        return rv
```

### cacheの話

もしかしたらcacheは不要かも。すでにjinja2側でキャシュしていない?
jinja2.loader.BaseLoaderのload()がキャッシュしてそう（追記、あーmtimeなどの指定がないからキャッシュされないかも）。


```python
class BaseLoader(object):
    """Baseclass for all loaders.  Subclass this and override `get_source` to
    implement a custom loading mechanism.  The environment provides a
    `get_template` method that calls the loader's `load` method to get the
    :class:`Template` object.

    A very basic example for a loader that looks up templates on the file
    system could look like this::

        from jinja2 import BaseLoader, TemplateNotFound
        from os.path import join, exists, getmtime

        class MyLoader(BaseLoader):

            def __init__(self, path):
                self.path = path

            def get_source(self, environment, template):
                path = join(self.path, template)
                if not exists(path):
                    raise TemplateNotFound(template)
                mtime = getmtime(path)
                with file(path) as f:
                    source = f.read().decode('utf-8')
                return source, path, lambda: mtime == getmtime(path)
    """

    #: if set to `False` it indicates that the loader cannot provide access
    #: to the source of templates.
    #:
    #: .. versionadded:: 2.4
    has_source_access = True

    def get_source(self, environment, template):
        """Get the template source, filename and reload helper for a template.
        It's passed the environment and template name and has to return a
        tuple in the form ``(source, filename, uptodate)`` or raise a
        `TemplateNotFound` error if it can't locate the template.

        The source part of the returned tuple must be the source of the
        template as unicode string or a ASCII bytestring.  The filename should
        be the name of the file on the filesystem if it was loaded from there,
        otherwise `None`.  The filename is used by python for the tracebacks
        if no loader extension is used.

        The last item in the tuple is the `uptodate` function.  If auto
        reloading is enabled it's always called to check if the template
        changed.  No arguments are passed so the function must store the
        old state somewhere (for example in a closure).  If it returns `False`
        the template will be reloaded.
        """
        if not self.has_source_access:
            raise RuntimeError('%s cannot provide access to the source' %
                               self.__class__.__name__)
        raise TemplateNotFound(template)

    def list_templates(self):
        """Iterates over all templates.  If the loader does not support that
        it should raise a :exc:`TypeError` which is the default behavior.
        """
        raise TypeError('this loader cannot iterate over all templates')

    @internalcode
    def load(self, environment, name, globals=None):
        """Loads a template.  This method looks up the template in the cache
        or loads one by calling :meth:`get_source`.  Subclasses should not
        override this method as loaders working on collections of other
        loaders (such as :class:`PrefixLoader` or :class:`ChoiceLoader`)
        will not call this method but `get_source` directly.
        """
        code = None
        if globals is None:
            globals = {}

        # first we try to get the source for this template together
        # with the filename and the uptodate function.
        source, filename, uptodate = self.get_source(environment, name)

        # try to load the code from the bytecode cache if there is a
        # bytecode cache configured.
        bcc = environment.bytecode_cache
        if bcc is not None:
            bucket = bcc.get_bucket(environment, name, filename, source)
            code = bucket.code

        # if we don't have code so far (not cached, no longer up to
        # date) etc. we compile the template
        if code is None:
            code = environment.compile(source, name, filename)

        # if the bytecode cache is available and the bucket doesn't
        # have a code so far, we give the bucket the new code and put
        # it back to the bytecode cache.
        if bcc is not None and bucket.code is None:
            bucket.code = code
            bcc.set_bucket(bucket)

        return environment.template_class.from_code(environment, code,
                                                    globals, uptodate)
```
