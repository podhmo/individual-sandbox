ipykernelの方にあった `from IPython.core.pylabtools import configure_inline_support, activate_matplotlib` という表記を元にpylabtools.pyを探して覗いてみると以下の様なものが定義されている。

```python

# If user specifies a GUI, that dictates the backend, otherwise we read the
# user's mpl default from the mpl rc structure
backends = {'tk': 'TkAgg',
            'gtk': 'GTKAgg',
            'gtk3': 'GTK3Agg',
            'wx': 'WXAgg',
            'qt4': 'Qt4Agg',
            'qt5': 'Qt5Agg',
            'qt': 'Qt5Agg',
            'osx': 'MacOSX',
            'nbagg': 'nbAgg',
            'notebook': 'nbAgg',
            'agg': 'agg',
            'svg': 'svg',
            'pdf': 'pdf',
            'ps': 'ps',
            'inline': 'module://ipykernel.pylab.backend_inline',
            'ipympl': 'module://ipympl.backend_nbagg',
            'widget': 'module://ipympl.backend_nbagg',
            }
```

なるほど。backendがinlineだと `module://ipykernel.pylab.backend_inline`になるつまり先程の(backend_inline.pyの)コードの此処が実行される

```python
ipykernel/pylab/backend_inline.py

```python
def _enable_matplotlib_integration():
    """Enable extra IPython matplotlib integration when we are loaded as the matplotlib backend."""
    from matplotlib import get_backend
    ip = get_ipython()
    backend = get_backend()
    if ip and backend == 'module://%s' % __name__:
        from IPython.core.pylabtools import configure_inline_support, activate_matplotlib
        try:
            activate_matplotlib(backend)
            configure_inline_support(ip, backend)
        except (ImportError, AttributeError):
            # bugs may cause a circular import on Python 2
            def configure_once(*args):
                activate_matplotlib(backend)
                configure_inline_support(ip, backend)
                ip.events.unregister('post_run_cell', configure_once)
            ip.events.register('post_run_cell', configure_once)

_enable_matplotlib_integration()
```

`configure_inline_support()` 自体はIPython.core.pylabtoolsの中にあって、以下の様な形(不要そうな条件分岐は取り除いている)。

```python
def configure_inline_support(shell, backend):
    from ipykernel.pylab.backend_inline import InlineBackend
    import matplotlib

    cfg = InlineBackend.instance(parent=shell)
    cfg.shell = shell
    if cfg not in shell.configurables:
        shell.configurables.append(cfg)

    from ipykernel.pylab.backend_inline import flush_figures
    shell.events.register('post_execute', flush_figures)

    # Save rcParams that will be overwrittern
    shell._saved_rcParams = {}
    for k in cfg.rc:
        shell._saved_rcParams[k] = matplotlib.rcParams[k]

    # load inline_rc
    matplotlib.rcParams.update(cfg.rc)
    new_backend_name = "inline"

    # only enable the formats once -> don't change the enabled formats (which the user may
    # has changed) when getting another "%matplotlib inline" call.
    # See https://github.com/ipython/ipykernel/issues/29
    cur_backend = getattr(configure_inline_support, "current_backend", "unset")
    if new_backend_name != cur_backend:
        # Setup the default figure format
        select_figure_formats(shell, cfg.figure_formats, **cfg.print_figure_kwargs)
        configure_inline_support.current_backend = new_backend_name
```

ipykernel.pylab.backend_inline.InlineBackendが使われているよということらしい。しかし正直な所、見てもよくわからない。

```python
class InlineBackend(InlineBackendConfig):
    """An object to store configuration of the inline backend."""

    # The typical default figure size is too large for inline use,
    # so we shrink the figure size to 6x4, and tweak fonts to
    # make that fit.
    rc = Dict({'figure.figsize': (6.0,4.0),
        # play nicely with white background in the Qt and notebook frontend
        'figure.facecolor': (1,1,1,0),
        'figure.edgecolor': (1,1,1,0),
        # 12pt labels get cutoff on 6x4 logplots, so use 10pt.
        'font.size': 10,
        # 72 dpi matches SVG/qtconsole
        # this only affects PNG export, as SVG has no dpi setting
        'figure.dpi': 72,
        # 10pt still needs a little more room on the xlabel:
        'figure.subplot.bottom' : .125
        },
        help="""Subset of matplotlib rcParams that should be different for the
        inline backend."""
    ).tag(config=True)

    figure_formats = Set({'png'},
                          help="""A set of figure formats to enable: 'png',
                          'retina', 'jpeg', 'svg', 'pdf'.""").tag(config=True)

    def _update_figure_formatters(self):
        if self.shell is not None:
            from IPython.core.pylabtools import select_figure_formats
            select_figure_formats(self.shell, self.figure_formats, **self.print_figure_kwargs)

    def _figure_formats_changed(self, name, old, new):
        if 'jpg' in new or 'jpeg' in new:
            if not pil_available():
                raise TraitError("Requires PIL/Pillow for JPG figures")
        self._update_figure_formatters()

    figure_format = Unicode(help="""The figure format to enable (deprecated
                                         use `figure_formats` instead)""").tag(config=True)

    def _figure_format_changed(self, name, old, new):
        if new:
            self.figure_formats = {new}

    print_figure_kwargs = Dict({'bbox_inches' : 'tight'},
        help="""Extra kwargs to be passed to fig.canvas.print_figure.

        Logical examples include: bbox_inches, quality (for jpeg figures), etc.
        """
    ).tag(config=True)
    _print_figure_kwargs_changed = _update_figure_formatters

    close_figures = Bool(True,
        help="""Close all figures at the end of each cell.

        When True, ensures that each cell starts with no active figures, but it
        also means that one must keep track of references in order to edit or
        redraw figures in subsequent cells. This mode is ideal for the notebook,
        where residual plots from other cells might be surprising.

        When False, one must call figure() to create new figures. This means
        that gcf() and getfigs() can reference figures created in other cells,
        and the active figure can continue to be edited with pylab/pyplot
        methods that reference the current active figure. This mode facilitates
        iterative editing of figures, and behaves most consistently with
        other matplotlib backends, but figure barriers between cells must
        be explicit.
        """).tag(config=True)
    
    shell = Instance('IPython.core.interactiveshell.InteractiveShellABC',
                     allow_none=True)

```
