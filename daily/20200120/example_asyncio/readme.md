# asyncioのsubprocess関連の情報をまとめる

## subprocess

[Subprocesses](https://docs.python.org/3/library/asyncio-subprocess.html)

- create_subprocess_exec()
- create_subprocess_shell()

ここにあるのは単純なそれこそsubprocess.run()のような使い勝手のもの。

## Process

こちらはsubprocess.Processっぽいが **何か** を一緒にしてくれているはず。と思ったらここはそうでもない。これを使うなにかがあるのだっけ（具体的に言うとprotocolとtransport）。

asyncio.subprocess.Process

```console
$ pyinspect inspect asyncio.subprocess:Process
asyncio.subprocess:Process <- builtins:object
    [method, OVERRIDE] __init__(self, transport, protocol, loop)
    [method, OVERRIDE] __repr__(self)
    [method] communicate(self, input=None)
        [method] _feed_stdin(self, input)
        [method] _noop(self)
        [method] _read_stream(self, fd)
        [method] wait(self)
    [method] kill(self)
    [property] returncode
    [method] send_signal(self, signal)
    [method] terminate(self)
```

## protocol, transport

冒頭の create_subprocess_exec などで transportとprotocol を作ってProcessに渡していた。ここでprotocol_factoryはasyncio.subprocess.SubprocessStreamProtocol。

```console
$ pyinspect inspect asyncio.subprocess:SubprocessStreamProtocol
asyncio.subprocess:SubprocessStreamProtocol <- asyncio.streams:FlowControlMixin <- asyncio.protocols:Protocol <- asyncio.protocols:SubprocessProtocol <- asyncio.protocols:BaseProtocol <- builtins:object
    [method, OVERRIDE] __init__(self, limit, loop)
    [method, OVERRIDE] __repr__(self)
    [method, OVERRIDE] _get_close_waiter(self, stream)
    [method, OVERRIDE] connection_made(self, transport)
    [method, OVERRIDE] pipe_connection_lost(self, fd, exc)
        [method] _maybe_close_transport(self)
    [method, OVERRIDE] pipe_data_received(self, fd, data)
    [method, OVERRIDE] process_exited(self)
        [method] _maybe_close_transport(self)

asyncio.streams:FlowControlMixin <- asyncio.protocols:Protocol <- asyncio.protocols:BaseProtocol <- builtins:object
    [method, OVERRIDE] __init__(self, loop=None)
    [method] _drain_helper(self)
    [method] _get_close_waiter(self, stream)
    [method, OVERRIDE] connection_lost(self, exc)
    [method, OVERRIDE] pause_writing(self)
    [method, OVERRIDE] resume_writing(self)

asyncio.protocols:Protocol <- asyncio.protocols:BaseProtocol <- builtins:object
    [method] data_received(self, data)
    [method] eof_received(self)

asyncio.protocols:SubprocessProtocol <- asyncio.protocols:BaseProtocol <- builtins:object
    [method] pipe_connection_lost(self, fd, exc)
    [method] pipe_data_received(self, fd, data)
    [method] process_exited(self)

asyncio.protocols:BaseProtocol <- builtins:object
    [method] connection_lost(self, exc)
    [method] connection_made(self, transport)
    [method] pause_writing(self)
    [method] resume_writing(self)
```

実際にtransportとprotocolを作るのはloop.subprocess_exec()。これはどこで定義されているかというとbase_events.pyのところ。

```console
$ pyinspect quote asyncio.base_events --lineno 1626
```


こういう感じ。


```python
class BaseEventLoop(events.AbstractEventLoop):

    
# ...

    def subprocess_exec(self, protocol_factory, program, *args,
                              stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                              stderr=subprocess.PIPE, universal_newlines=False,
                              shell=False, bufsize=0,
                              encoding=None, errors=None, text=None,
                              **kwargs):
# ...

            debug_log = f'execute program {program!r}'
            self._log_subprocess(debug_log, stdin, stdout, stderr)
        transport = await self._make_subprocess_transport(
            protocol, popen_args, False, stdin, stdout, stderr,
            bufsize, **kwargs)
```

そして `_make_subprocess_transport` でtransportが作られている。protocolは単にfactoryを呼び出しているだけ。

```console
$ grep _make_subprocess_transport -r . | grep def
./base_events.py:    async def _make_subprocess_transport(self, protocol, args, shell,
./unix_events.py:    async def _make_subprocess_transport(self, protocol, args, shell,
./windows_events.py:    async def _make_subprocess_transport(self, protocol, args, shell,
```

そして中を覗いてみると、以下の様なメソッドやクラスが呼ばれている。

- get_child_watcher()
- watcher.add_child_handler()
- _UnixSubprocessTransport

asyncio/unix_events.py

```
class _UnixSelectorEventLoop(selector_events.BaseSelectorEventLoop):
    """Unix event loop."""
# ...

    def _make_subprocess_transport(self, protocol, args, shell,
                                         stdin, stdout, stderr, bufsize,
                                         extra=None, **kwargs):
        with events.get_child_watcher() as watcher:
            if not watcher.is_active():
                # Check early.
                # Raising exception before process creation
                # prevents subprocess execution if the watcher
                # is not ready to handle it.
                raise RuntimeError("asyncio.get_child_watcher() is not activated, "
                                   "subprocess support is not installed.")
            waiter = self.create_future()
            transp = _UnixSubprocessTransport(self, protocol, args, shell,
                                              stdin, stdout, stderr, bufsize,
                                              waiter=waiter, extra=extra,
                                              **kwargs)

            watcher.add_child_handler(transp.get_pid(),
                                      self._child_watcher_callback, transp)
            try:
                await waiter
            except (SystemExit, KeyboardInterrupt):
                raise
            except BaseException:
                transp.close()
                await transp._wait()
                raise

        return transp
```

記憶確かならwatcherはいくつかある。どのwatcherを取るかはPolicyオブジェクトに由来する。Policyオブジェクトというのは。

```console
$ pyinspect inspect asyncio.unix_events:_UnixDefaultEventLoopPolicy
asyncio $ pyinspect inspect asyncio.unix_events:_UnixDefaultEventLoopPolicy
asyncio.unix_events:_UnixDefaultEventLoopPolicy <- asyncio.events:BaseDefaultEventLoopPolicy <- asyncio.events:AbstractEventLoopPolicy <- builtins:object
    [method, OVERRIDE] __init__(self)
    [method, OVERRIDE] get_child_watcher(self)
        [method] _init_watcher(self)
    [method, OVERRIDE] set_child_watcher(self, watcher)
    [method, OVERRIDE] set_event_loop(self, loop)

asyncio.events:BaseDefaultEventLoopPolicy <- asyncio.events:AbstractEventLoopPolicy <- builtins:object
    [method, OVERRIDE] __init__(self)
    [method, OVERRIDE] get_event_loop(self)
        [method, OVERRIDE] set_event_loop(self, loop)
        [method, OVERRIDE] new_event_loop(self)

asyncio.events:AbstractEventLoopPolicy <- builtins:object
    [method] get_child_watcher(self)
    [method] get_event_loop(self)
    [method] new_event_loop(self)
    [method] set_child_watcher(self, watcher)
    [method] set_event_loop(self, loop)
```

ここでget_child_watcherが何を使うかというのが気になるところ。

### trasport

_make_read_pipe_transport

```console
$ pyinspect quote asyncio.base_events --lineno=1521
```

このconnect_read_pipe()で使われているがどこで呼ばれるのだろう？

base_events.py 

```python
class BaseEventLoop(events.AbstractEventLoop):

    
# ...

    def connect_read_pipe(self, protocol_factory, pipe):
        protocol = protocol_factory()
        waiter = self.create_future()

        transport = self._make_read_pipe_transport(pipe, protocol, waiter)
        try:
```


答えはtransport。この`_connect_pipes()`で使われる

```console
$ pyinspect inspect asyncio.unix_events:_UnixSubprocessTransport
asyncio.unix_events:_UnixSubprocessTransport <- asyncio.base_subprocess:BaseSubprocessTransport <- asyncio.transports:SubprocessTransport <- asyncio.transports:BaseTransport <- builtins:object
    [method, OVERRIDE] _start(self, args, shell, stdin, stdout, stderr, bufsize, **kwargs)

asyncio.base_subprocess:BaseSubprocessTransport <- asyncio.transports:SubprocessTransport <- asyncio.transports:BaseTransport <- builtins:object
    [method] __del__(self, _warn=<built-in function warn>)
        [method, OVERRIDE] close(self)
    [method, OVERRIDE] __init__(self, loop, protocol, args, shell, stdin, stdout, stderr, bufsize, waiter=None, extra=None, **kwargs)
        [method] _start(self, args, shell, stdin, stdout, stderr, bufsize, **kwargs)
        [method] _connect_pipes(self, waiter)
        [method, OVERRIDE] close(self)
    [method, OVERRIDE] __repr__(self)
    [method] _pipe_connection_lost(self, fd, exc)
        [method] _call(self, cb, *data)
        [method] _try_finish(self)
            [method] _call(self, cb, *data)
            [method] _call_connection_lost(self, exc)
    [method] _pipe_data_received(self, fd, data)
        [method] _call(self, cb, *data)
    [method] _process_exited(self, returncode)
        [method] _call(self, cb, *data)
        [method] _try_finish(self)
            [method] _call(self, cb, *data)
            [method] _call_connection_lost(self, exc)
    [method] _wait(self)
    [method, OVERRIDE] get_pid(self)
    [method, OVERRIDE] get_pipe_transport(self, fd)
    [method, OVERRIDE] get_protocol(self)
    [method, OVERRIDE] get_returncode(self)
    [method, OVERRIDE] is_closing(self)
    [method, OVERRIDE] kill(self)
        [method] _check_proc(self)
    [method, OVERRIDE] send_signal(self, signal)
        [method] _check_proc(self)
    [method, OVERRIDE] set_protocol(self, protocol)
    [method, OVERRIDE] terminate(self)
        [method] _check_proc(self)

asyncio.transports:SubprocessTransport <- asyncio.transports:BaseTransport <- builtins:object
    [method] get_pid(self)
    [method] get_pipe_transport(self, fd)
    [method] get_returncode(self)
    [method] kill(self)
    [method] send_signal(self, signal)
    [method] terminate(self)

asyncio.transports:BaseTransport <- builtins:object
    [method, OVERRIDE] __init__(self, extra=None)
    [method] close(self)
    [method] get_extra_info(self, name, default=None)
    [method] get_protocol(self)
    [method] is_closing(self)
    [method] set_protocol(self, protocol)
```
