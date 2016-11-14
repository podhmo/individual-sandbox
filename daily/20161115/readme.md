# golang pointer of interface

hmm

# python 依存した関数の定義の呼び出しで引数を増やしたい

wip

```python
class WriterContext(object):
    def __init__(self, cont, parent=None):
        self.argspec = LazyArguments()
        self.retspec = LazyArguments()
        self.callspec = LazyArguments()
        self.parent = parent
        self.cont = cont

    @property
    def is_toplevel(self):
        return self.parent is None

    def new_child(self):
        return self.__class__(self.cont, parent=self)

    def as_retspec(self, value):
        self.retspec.add_arg(value)
        return self.retspec

    def as_argspec(self, arg):
        self.argspec.add_arg(arg)
        return self.argspec

    def as_callspec(self, arg):
        self.callspec.add_arg(arg)
        return self.callspec

    def add_extra_argspec(self, arg):
        self.argspec.add_arg(arg)
        self.parent.add_argspec(arg)

    def add_extra_callspec(self, arg):
        self.callspec.add_arg(arg)
        self.parent.add_callspec(arg)

    def add_cont(self, fn):
        self.cont.append(fn)


class LazyArguments(object):
    def __init__(self, value):
        self.args = [value]
        self.cache = set(self.args)

    def add_arg(self, arg):
        if arg not in self.cache:
            self.cache(arg)
            self.args.append(arg)

    def __str__(self):
        return ", ".join(self.args)
````
