## jqの不思議

- リダイレクトすると文字の色が消える
- コンソール上では文字に色が付く

```console
$ git clone --depth=1 git@github.com:stedolan/jq.git
$ cd jq
$ autoreconf -fi
$ ./configure --with-oniguruma=builtin
$ make
```

- https://github.com/stedolan/jq/issues/1659

## python matplotlib inlineでのplot

IPython/core/events.py

```python
class EventManager(object):
    def register(self, event, function):
        if not callable(function):
            raise TypeError('Need a callable, got %r' % function)
        callback_proto = available_events.get(event)
```

ここでprintすれば楽そう。

```
[NbConvertApp] Converting notebook 03nbreversible.ipynb to markdown
[NbConvertApp] Executing notebook with kernel: python

%matplotlib inline
from matplotlib import pyplot as plt
xs = [1, 2, 3, 4]
plt.plot(xs)
    @@@ post_run_cell <function _enable_matplotlib_integration.<locals>.configure_once at 0x7f7c765572f0> ipykernel.pylab.backend_inline
    @@@ post_execute <function install_repl_displayhook.<locals>.post_execute at 0x7f7c7655b730> matplotlib.pyplot
    @@@ post_execute <function flush_figures at 0x7f7c765571e0> ipykernel.pylab.backend_inline
    [<matplotlib.lines.Line2D at 0x7f7c74295630>]
![png](03nbreversible_files/03nbreversible_0_2.png)
    @@@ post_execute <function flush_figures at 0x7f7c765571e0> ipykernel.pylab.backend_inline
```

## python nbreversibleの中でpdbを読んだ時にエラーが出る

```
StdinNotImplementedError: raw_input was called, but this frontend does not support input requests.
StdinNotImplementedError: raw_input was called, but this frontend does not support input requests.
```

何かallow_stdinをtrueにするといけそう？

- [Messaging in Jupyter — jupyter_client 6.0.0.dev documentation](https://jupyter-client.readthedocs.io/en/stable/messaging.html "Messaging in Jupyter — jupyter_client 6.0.0.dev documentation")

### 追記

最終的にself見るだけでよかった。はい。

```
@@@ <ipykernel.zmqshell.ZMQInteractiveShell object at 0x7f6656bfc240> @@@
```

## まじめにfont変えてみる?

- https://www.rs.tus.ac.jp/yyusa/ricty.html
- http://levien.com/type/myfonts/inconsolata.html


## matplotlib sixel

- https://github.com/kktk-KO/sixelplot
- https://github.com/saitoha/PySixel
- https://github.com/koppa/matplotlib-sixel

```
$ pip install PySixel
...
    sixel/sixel_cimpl.c:8094:21: error: ‘PyThreadState’ {aka ‘struct _ts’} has no member named ‘exc_type’; did you mean ‘curexc_type’?
         *type = tstate->exc_type;
                         ^~~~~~~~
                         curexc_type
```

- https://github.com/pygame/pygame/issues/382
- https://github.com/cython/cython/issues/1955

cythonの問題っぽいな。

```
$ git clone git@github.com:saitoha/pysixel
$ cd pysixel
# いろいろいじる
$ make build
$ make install
```

そもそもsixel対応しているものが存在していない

[billiob/terminology: The best terminal emulator based on the Enlightenment Foundation Libraries](https://github.com/billiob/terminology "billiob/terminology: The best terminal emulator based on the Enlightenment Foundation Libraries")

## python IPython

途中から立ち上げるのは、IPython.embedで良い。

```
from IPython import embed

xs = [1, 2, 3, 4]

embed()
In [1] xs
Out[2] [1, 2, 3, 4]
```

### matplotlib inline

IPythonなどでやると、UnknownBackendが出る

```
UnknownBackend: No event loop integration for 'inline'. Supported event loops are: qt, qt4, qt5, gtk, gtk2, gtk3, tk, wx, pyglet, glut, osx
```

ちなみにjupyter-terminalだとinlineでは動くがそこに直接表示はされない。

これはshellのenable_matplotlib()関連の挙動。-> enable_gui() -> get_inputhook_name_and_func()

```
~/venvs/my/lib/python3.7/site-packages/IPython/core/interactiveshell.py in enable_matplotlib(self, gui)
   3052         # Now we must activate the gui pylab wants to use, and fix %run to take
   3053         # plot updates into account
-> 3054         self.enable_gui(gui)
   3055         self.magics_manager.registry['ExecutionMagics'].default_runner = \
   3056             pt.mpl_runner(self.safe_execfile)

~/venvs/my/lib/python3.7/site-packages/IPython/terminal/interactiveshell.py in enable_gui(self, gui)
    504         if gui:
    505             self.active_eventloop, self._inputhook =\
--> 506                 get_inputhook_name_and_func(gui)
    507         else:
    508             self.active_eventloop = self._inputhook = None

~/venvs/my/lib/python3.7/site-packages/IPython/terminal/pt_inputhooks/__init__.py in get_inputhook_name_and_func(gui)
     36 
     37     if gui not in backends:
---> 38         raise UnknownBackend(gui)
     39 
     40     if gui in aliases:
```

enable_gui()たしかに呼ばれている。

```python
class InteractiveShell(SingletonConfigurable):
    """An enhanced, interactive shell for Python."""

    def enable_gui(self, gui=None):
        raise NotImplementedError('Implement enable_gui in a subclass')
    
    def enable_matplotlib(self, gui=None):

        from IPython.core import pylabtools as pt
        gui, backend = pt.find_gui_and_backend(gui, self.pylab_gui_select)
    
        if gui != 'inline':
            # If we have our first gui selection, store it
            if self.pylab_gui_select is None:
                self.pylab_gui_select = gui
            # Otherwise if they are different
            elif gui != self.pylab_gui_select:
                print('Warning: Cannot change to a different GUI toolkit: %s.'
                        ' Using %s instead.' % (gui, self.pylab_gui_select))
                gui, backend = pt.find_gui_and_backend(self.pylab_gui_select)
        
        pt.activate_matplotlib(backend)
        pt.configure_inline_support(self, backend)
        
        # Now we must activate the gui pylab wants to use, and fix %run to take
        # plot updates into account
        self.enable_gui(gui)
        self.magics_manager.registry['ExecutionMagics'].default_runner = \
            pt.mpl_runner(self.safe_execfile)
        
        return gui, backend
```

ちなみに、jupyter-consoleから呼ぶと以下の様な形で呼ばれる。

```
-> get_ipython().run_line_magic('matplotlib', 'inline')
  $HOME/venvs/viz/lib/python3.7/site-packages/IPython/core/interactiveshell.py(2131)run_line_magic()
-> result = fn(*args,**kwargs)
  <decorator-gen-107>(2)matplotlib()
  $HOME/venvs/viz/lib/python3.7/site-packages/IPython/core/magic.py(187)<lambda>()
-> call = lambda f, *a, **k: f(*a, **k)
  $HOME/venvs/viz/lib/python3.7/site-packages/IPython/core/magics/pylab.py(99)matplotlib()
-> gui, backend = self.shell.enable_matplotlib(args.gui)
  $HOME/venvs/viz/lib/python3.7/site-packages/IPython/core/interactiveshell.py(3055)enable_matplotlib()
-> self.enable_gui(gui)
> $HOME/venvs/viz/lib/python3.7/site-packages/ipykernel/zmqshell.py(484)enable_gui()
-> from .eventloops import enable_gui as real_enable_gui
```

ipykernel/zmqshell.py

```python
class ZMQInteractiveShell(InteractiveShell):
    """A subclass of InteractiveShell for ZMQ."""

    # Over ZeroMQ, GUI control isn't done with PyOS_InputHook as there is no
    # interactive input being read; we provide event loop support in ipkernel
    def enable_gui(self, gui):
        from .eventloops import enable_gui as real_enable_gui
        try:
            real_enable_gui(gui)
            self.active_eventloop = gui
        except ValueError as e:
            raise UsageError("%s" % e)
```

そしてこれらはどこでもこれを呼ぶ。

ipykernel/eventloops.py

```python
def enable_gui(gui, kernel=None):
    """Enable integration with a given GUI"""
    if gui not in loop_map:
        e = "Invalid GUI request %r, valid ones are:%s" % (gui, loop_map.keys())
        raise ValueError(e)
    if kernel is None:
        if Application.initialized():
            kernel = getattr(Application.instance(), 'kernel', None)
        if kernel is None:
            raise RuntimeError("You didn't specify a kernel,"
                " and no IPython Application with a kernel appears to be running."
            )
    loop = loop_map[gui]
    if loop and kernel.eventloop is not None and kernel.eventloop is not loop:
        raise RuntimeError("Cannot activate multiple GUI eventloops")
    kernel.eventloop = loop
```

さて、nbconvertでexecuteしたときはどうなるのか。


