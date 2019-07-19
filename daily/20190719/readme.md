## python のpstats的な情報をみるviewerを作るのも良いのかも？

- viewerは何で作るのが良いんだろう？

## python line_profiler まじめに中身を理解する

- https://github.com/rkern/line_profiler
- https://docs.python.org/ja/3/c-api/index.html

気になること

- 依存関係は
- 何がどこで定義している？
- どうやって%をだしているの？

### setup.pyを見る

install_requires

- ipython

build部分

- Cython.Distutils.build_ext() をimportしてる
- `_line_profiler.pyx` が使えたらそちらを、そうでない場合は `_line_profiler.c`

modules

- kernprof
- line_profiler

ext modules

distutils.extension.Extnsionを使って `_line_profiler`

- _line_profiler.pyx
- timers.c
- unset_trace.c

を利用しているっぽい。

entry points

- `kernprof=kernprof:main`

### その前に使用方法を整理

```console
$ kernprof -l <file>
$ python -m line_profiler <outfile>.lprof
```

### kernprofで出力される .lprofの構造

line_profilerはCではない？もしかして。

```
py_modules = ['line_profiler', 'kernprof']


setup(
...
    py_modules = py_modules,
)
```

まじめに中を覗いて見るか。optparseが使われているし。古めのコード。

### main

```python
def main():
    usage = "usage: %prog profile.lprof"
    parser = optparse.OptionParser(usage=usage, version='%prog 1.0b2')

    options, args = parser.parse_args()
    if len(args) != 1:
        parser.error("Must provide a filename.")
    lstats = load_stats(args[0])
    show_text(lstats.timings, lstats.unit)
```

- load_stats
- show_text

を見れば良さそう。

### load_stats()

load_stats()はpickle.loadしているだけ。

### show_text()

```
    lstats = load_stats(args[0])
    show_text(lstats.timings, lstats.unit)
```

ふつうに内部はlinecacheとinspectで表示しているだけだな。

```
    all_lines = linecache.getlines(filename)
    sublines = inspect.getblock(all_lines[start_lineno - 1 :])
```

inspect.getblock()そういえば忘れてた。いい感じに完了まで表示するのに便利かも。

### わからないこと(exec)

これどういう意味なんだろう？execだけ取り出す。

```python
    import builtins
    exec_ = getattr(builtins, "exec")
    del builtins
```

これは、ここで使われている。

~/venvs/my/lib/python3.7/site-packages/line_profiler.py

```python
class LineProfiler(CLineProfiler):
    """A profiler that records the execution times of individual lines.
    """

# ...

    def runctx(self, cmd, globals, locals):
        """Profile a single executable statement in the given namespaces.
        """

# ...

        self.enable_by_count()
        try:
            exec_(cmd, globals, locals)
        finally:
            self.disable_by_count()
```

codeをとりだして一個一個実行しているのかな。

### inspect

```console
$ pyinspect inspect line_profiler
[function] is_coroutine(f)
----------------------------------------
[function] is_generator(f)
----------------------------------------
line_profiler:LineProfiler <- _line_profiler:LineProfiler <- builtins:object
    [method, OVERRIDE] __call__(self, func)
        [method] wrap_coroutine(self, func)
        [method] wrap_generator(self, func)
        [method] wrap_function(self, func)
    [method] add_module(self, mod)
    [method] dump_stats(self, filename)
    [method] print_stats(self, stream=None, output_unit=None, stripzeros=False)
    [method] run(self, cmd)
        [method] runctx(self, cmd, globals, locals)
    [method] runcall(self, func, *args, **kw)

_line_profiler:LineProfiler <- builtins:object
    [method] __enter__
    [method] __exit__
    [method, OVERRIDE] __init__(self, /, *args, **kwargs)
    [static method, OVERRIDE] __new__(*args, **kwargs)
    [method, OVERRIDE] __reduce__
    [method] __setstate__
    [method] add_function
    [method] disable
    [method] disable_by_count
    [method] enable
    [method] enable_by_count
    [method] get_stats

----------------------------------------
[function] show_func(filename, start_lineno, func_name, timings, unit, output_unit=None, stream=None, stripzeros=False)
----------------------------------------
[function] show_text(stats, unit, output_unit=None, stream=None, stripzeros=False)
    [function] show_func(filename, start_lineno, func_name, timings, unit, output_unit=None, stream=None, stripzeros=False)
----------------------------------------
line_profiler:LineProfilerMagics <- IPython.core.magic:Magics <- traitlets.config.configurable:Configurable <- traitlets.traitlets:HasTraits <- traitlets.traitlets:HasDescriptors <- builtins:object
    [method] lprun(self, parameter_s='')

IPython.core.magic:Magics <- traitlets.config.configurable:Configurable <- traitlets.traitlets:HasTraits <- traitlets.traitlets:HasDescriptors <- builtins:object
    [method, OVERRIDE] __init__(self, shell=None, **kwargs)
    [method] arg_err(self, func)
    [method] default_option(self, fn, optstr)
    [method] format_latex(self, strng)
    [method] parse_options(self, arg_str, opt_str, *long_opts, **kw)

traitlets.config.configurable:Configurable <- traitlets.traitlets:HasTraits <- traitlets.traitlets:HasDescriptors <- builtins:object
    [method, OVERRIDE] __init__(self, **kwargs)
        [method] _load_config(self, cfg, section_names=None, traits=None)
            [method] _find_my_config(self, cfg)
                [class method] section_names()
            [class method] section_names()
    [method] _config_changed
    [class method] class_config_rst_doc()
    [class method] class_config_section()
    [class method] class_print_help(inst=None)
        [class method] class_get_help(inst=None)
            [class method] class_get_trait_help(trait, inst=None)
    [method] update_config(self, config)
        [method] _load_config(self, cfg, section_names=None, traits=None)
            [method] _find_my_config(self, cfg)
                [class method] section_names()
            [class method] section_names()

traitlets.traitlets:HasTraits <- traitlets.traitlets:HasDescriptors <- builtins:object
    [method] __getstate__(self)
    [method, OVERRIDE] __init__(self, *args, **kwargs)
        [method] hold_trait_notifications(self)
            [method] notify_change(self, change)
            [method] set_trait(self, name, value)
                [method] has_trait(self, name)
        [method] has_trait(self, name)
    [method] __setstate__(self, state)
    [method] _notify_trait(self, name, old_value, new_value)
        [method] notify_change(self, change)
    [method] _register_validator(self, handler, names)
    [method] add_traits(self, **traits)
    [class method] class_own_trait_events(name)
    [class method] class_own_traits(**metadata)
        [class method] class_traits(**metadata)
    [class method] class_trait_names(**metadata)
        [class method] class_traits(**metadata)
    [property] cross_validation_lock
    [method] on_trait_change(self, handler=None, name=None, remove=False)
        [method] unobserve(self, handler, names=traitlets.All, type='change')
            [method] _remove_notifiers(self, handler, name, type)
        [method] observe(self, handler, names=traitlets.All, type='change')
            [method] _add_notifiers(self, handler, name, type)
    [method, OVERRIDE] setup_instance(self, *args, **kwargs)
    [class method] trait_events(name=None)
        [method] trait_names(self, **metadata)
            [method] traits(self, **metadata)
    [method] trait_metadata(self, traitname, key, default=None)
    [method] unobserve_all(self, name=traitlets.All)

traitlets.traitlets:HasDescriptors <- builtins:object
    [static method, OVERRIDE] __new__(cls, *args, **kwargs)
    [method] setup_instance(self, *args, **kwargs)

----------------------------------------
[function] load_ipython_extension(ip)
----------------------------------------
[function] load_stats(filename)
----------------------------------------
[function] main()
    [function] load_stats(filename)
    [function] show_text(stats, unit, output_unit=None, stream=None, stripzeros=False)
        [function] show_func(filename, start_lineno, func_name, timings, unit, output_unit=None, stream=None, stripzeros=False)
----------------------------------------
```

### kernprof

どちらかと言うとkernprofの方が重要かも？内部でたぶんline_profilerが使われているんだろうけれど。

```
$ pyinspect inspect kernprof.py
[function] execfile(filename, globals=None, locals=None)
    [function] exec_(source, globals=None, locals=None, /)
----------------------------------------
[function] is_generator(f)
----------------------------------------
[function] find_script(script_name)
----------------------------------------
[function] main(args=None)
    [function] find_script(script_name)
    [function] execfile(filename, globals=None, locals=None)
        [function] exec_(source, globals=None, locals=None, /)
    [class] ContextualProfile(*args, **kwds)
        [function] is_generator(f)
----------------------------------------
```

find_scriptしてexecfileする感じ？

- line_by_line 以外のものもあるのか
- setupfileがあればそれをexecしている
- ふつうにprofilerを作って(line_by_lineならLineProfiler)

hmm以下の違い

- LineProfiler
- ContextualProfile

