## python jupyter notebookを扱う(kernel)

- jupyter-client?

```console
$ jupyter kernelspec list
Available kernels:
  python3    VENV/share/jupyter/kernels/python3
$ jupyter console --kernel python3
```

### packageとしては?

- jupyter_core?
- jupyter_client?

### テキトウにkernelspecappを読む

```
class KernelSpecApp(Application):
    subcommands = Dict({
        'list': (ListKernelSpecs, ListKernelSpecs.description.splitlines()[0]),
        'install': (InstallKernelSpec, InstallKernelSpec.description.splitlines()[0]),
        'uninstall': (RemoveKernelSpec, "Alias for remove"),
        'remove': (RemoveKernelSpec, RemoveKernelSpec.description.splitlines()[0]),
        'install-self': (InstallNativeKernelSpec, InstallNativeKernelSpec.description.splitlines()[0]),
    })

    def start(self):
        return self.subapp.start()

```


list見れば良さそう

```
class ListKernelSpecs(JupyterApp):
    def start(self):
        paths = self.kernel_spec_manager.find_kernel_specs()
        specs = self.kernel_spec_manager.get_all_specs()
        for kernelname, path in sorted(paths.items(), key=path_key):
            print("  %s    %s" % (kernelname.ljust(name_len), path))

```

直接scriptで実行してみる。configがなければ直接インスタンスを作成してstartで良さそう。

```python
from jupyter_client.kernelspecapp import ListKernelSpecs

app = ListKernelSpecs()
app.start()
# -- stdout --------------------
# >> Available kernels:
# >>   python3    VENV/share/jupyter/kernels/python3
```

ただしlaunch_instance経由で使うのが正しい模様。

```python
from jupyter_client.kernelspecapp import ListKernelSpecs

ListKernelSpecs.launch_instance()
```

### replを作成することを期待してkernelappを使う。

```python
from jupyter_client.kernelapp import main

# main = KernelApp.launch_instance こうなっているのでmainを使っても良い
main()
```

勝手に動き出すのでどこに生成されるかの情報が欲しい

```
[KernelApp] Starting kernel 'python3'
[KernelApp] Connection file: /run/user/1000/jupyter/kernel-5b636ca2-4c1e-478f-9a79-3c51d72a9bea.json
[KernelApp] To connect a client: --existing kernel-5b636ca2-4c1e-478f-9a79-3c51d72a9bea.json
```

ところで直接jupyterコマンドで

```
jupyter kernel --debug --kernel python3 --ip=""
# ipはunix domain socketもいける？
```

以下のようなことをしている。

```python
class KernelApp(JupyterApp):

    def initialize(self, argv=None):
        super(KernelApp, self).initialize(argv)
        self.km = KernelManager(kernel_name=self.kernel_name,
                                config=self.config)
        cf_basename = 'kernel-%s.json' % uuid.uuid4()
        self.km.connection_file = os.path.join(self.runtime_dir, cf_basename)
        self.loop = IOLoop.current()
        self.loop.add_callback(self._record_started)

    def start(self):
        self.log.info('Starting kernel %r', self.kernel_name)
        try:
            self.km.start_kernel()
            self.log_connection_info()
            self.setup_signals()
            self.loop.start()
        finally:
            self.km.cleanup()
```

loopを無視したらKernelmanagerで良いのでは？良さそう。まぁsignalのhandlingなどをしているので素直にappを使うのが良い。
設定可能なオプションを全部見るには `--help` ではなく `--help-all` を使う。

```console
$ jupyter kernel --help-all

--KernelManager.connection_file=<Unicode>
    Default: ''
    JSON file in which to store connection info [default: kernel-<pid>.json]
    This file will contain the IP, ports, and authentication key needed to
    connect clients to this kernel. By default, this file will be created in the
    security dir of the current profile, but can be specified by absolute path.
```

まだ先は長そう。managerからclientを作るとhas_kerrnelで怒られる？

```
Traceback (most recent call last):
  File "05client.py", line 18, in <module>
    client.wait_for_ready(timeout=3)
  File "VENV/lib/python3.7/site-packages/jupyter_client/blocking/client.py", line 120, in wait_for_ready
    raise RuntimeError('Kernel died before replying to kernel_info')
RuntimeError: Kernel died before replying to kernel_info
```

ここ。

```python
class KernelClient(ConnectionFileMixin):
    def is_alive(self):
        """Is the kernel process still running?"""
        from .manager import KernelManager
        if isinstance(self.parent, KernelManager):
            # This KernelClient was created by a KernelManager,
            # we can ask the parent KernelManager:
            breakpoint()
            return self.parent.is_alive()
        if self._hb_channel is not None:
            # We don't have access to the KernelManager,
            # so we use the heartbeat.
            return self._hb_channel.is_beating()
        else:
            # no heartbeat and not local, we can't tell if it's running,
            # so naively return True
            return True


class KernelManager(ConnectionFileMixin):
    def is_alive(self):
        """Is the kernel process still running?"""
        if self.has_kernel:
            if self.kernel.poll() is None:
                return True
            else:
                return False
        else:
            # we don't have a kernel
            return False
```

### 落ち穂拾い

- 接続情報はどこにある？
- どのタイミングで `/run/user/1000/*.json`のdirectoryが決まる？
- logの設定はどうする？
- `main = xxxx.lanucn_instanceの意味`
- 認証を良い感じに扱いたい
- tcpで立ち上げない方法 (jupyter kernel)
- 結果をipynbにdumpする方法

#### 接続情報

jupyter_client/connect.py。ConnectionFileMixinを見ていけば良い。
実際にはそれぞれのロジックがメソッド名と同じ関数として書かれている。
（メソッドはほとんどfacade）

```
jupyter_client.connect:ConnectionFileMixin <- traitlets.config.configurable:LoggingConfigurable <- traitlets.config.configurable:Configurable <- traitlets.traitlets:HasTraits <- traitlets.traitlets:HasDescriptors <- builtins:object
    [method] blocking_client(self)
    [method] cleanup_ipc_files(self)
    [method] cleanup_random_ports(self)
    [method] connect_control(self, identity=None)
    [method] connect_hb(self, identity=None)
    [method] connect_iopub(self, identity=None)
    [method] connect_shell(self, identity=None)
    [method] connect_stdin(self, identity=None)
    [method] load_connection_file(self, connection_file=None)
    [method] write_connection_file(self)
```

write_connection_file()をしたあとでなければget_connection_infoの意味がない。
ファイル名は self.connection_fileに保存される。

#### `/run/users/1000/*.json`

以下の２つの関数でdefaultが異なる。

- find_connection_file()
- write_connection_file()

```
OSError: Could not find 'tmpkpv4qky9.json' in ['.', '/run/user/1000/jupyter']
```

どうもwrite時にはdefaultではtmpに置き、find時には `jupyter_runtime_dir()` を尊重するみたい。

ただ `jupyter kernel` はKernelAppが `initialize()` のタイミングで決めてしまうみたい。

```python
class KernelApp(JupyterApp):

    def initialize(self, argv=None):
# ...
        cf_basename = 'kernel-%s.json' % uuid.uuid4()
        self.km.connection_file = os.path.join(self.runtime_dir, cf_basename)
```

#### log

logの設定。このself.logをどうやって使うか。

```python
class _:
    def start(self):
        self.log.info('Starting kernel %r', self.kernel_name)
```

このクラスがMixinされていたので使えた設定の模様。

```python
class LoggingConfigurable(Configurable):
    """A parent class for Configurables that log.

    Subclasses have a log trait, and the default behavior
    is to get the logger from the currently running Application.
    """

    log = Instance('logging.Logger')
    @default('log')
    def _log_default(self):
        from traitlets import log
        return log.get_logger()
```

traitletsの使いかた

```
import getpass

class Identity(HasTraits):
    username = Unicode()

    @default('username')
    def _default_username(self):
        return getpass.getuser()

app = Identity(username="foo")
```

- https://traitlets.readthedocs.io/en/stable/using_traitlets.html

コンソールから使うときにはこんな感じで(`--help-all`で確認可能)

```
--KernelApp.log_datefmt=<Unicode>
    Default: '%Y-%m-%d %H:%M:%S'
    The date format used by logging formatters for %(asctime)s
--KernelApp.log_format=<Unicode>
    Default: '[%(name)s]%(highlevel)s %(message)s'
    The Logging format template
--KernelApp.log_level=<Enum>
    Default: 30
    Choices: (0, 10, 20, 30, 40, 50, 'DEBUG', 'INFO', 'WARN', 'ERROR', 'CRITICAL')
    Set the log level by value or name.
```

#### main

以下の意味。

```
main = KernelApp.launch_instance
```

これは、traitletsを覗くとわかった記憶。

```
traitlets.config.application:Application <- traitlets.config.configurable:SingletonConfigurable <- traitlets.config.configurable:LoggingConfigurable <- traitlets.config.configurable:Configurable <- traitlets.traitlets:HasTraits <- traitlets.traitlets:HasDescriptors <- builtins:object
    [class method] launch_instance(argv=None, **kwargs)
```

hmmふつうにクラスを作成すると特に値が設定されない。

```python
class JupyterApp(Application):
    @classmethod
    def launch_instance(cls, argv=None, **kwargs):
        """Launch an instance of a Jupyter Application"""
        try:
            return super(JupyterApp, cls).launch_instance(argv=argv, **kwargs)
        except NoStart:
            return

class Application(SingletonConfigurable):
    @classmethod
    def launch_instance(cls, argv=None, **kwargs):
        """Launch a global instance of this Application

        If a global instance already exists, this reinitializes and starts it
        """
        app = cls.instance(**kwargs)
        app.initialize(argv)
        app.start()
```

#### tcp 以外の利用

はい。

```
--KernelManager.transport=<CaselessStrEnum>
    Default: 'tcp'
    Choices: ['tcp', 'ipc']
```

### kernelの挙動

なんかおかしい。


## jupyter run_kernelするだけ

```python
from jupyter_client import run_kernel
```

## kernel managerからkernelを動かす時にextra_argumentsを指定する。

方法がない。

```python
class KernelManager(ConnectionFileMixin):

    def start(self):
        self.log.info('Starting kernel %r', self.kernel_name)
        try:
            self.km.start_kernel()  # <- 
            self.log_connection_info()
            self.setup_signals()
            self.loop.start()
        finally:
            self.km.cleanup()


class KernelManager(ConnectionFileMixin):

    def start_kernel(self, **kw):
# ...
        extra_arguments = kw.pop('extra_arguments', [])
```

