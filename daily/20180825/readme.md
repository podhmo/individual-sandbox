## jupyter kernel

そのまま立ち上げてclientで通信できないんだっけ？

```console
$ jupyter-kernel
[KernelApp] Starting kernel 'python3'
[KernelApp] Connection file: /run/user/1000/jupyter/kernel-91949ec0-0f35-4f29-98e9-8c8b4931175c.json
[KernelApp] To connect a client: --existing kernel-91949ec0-0f35-4f29-98e9-8c8b4931175c.json
```

/run/user/1000/jupyter/kernel-91949ec0-0f35-4f29-98e9-8c8b4931175c.json
```
{
  "shell_port": 43181,
  "iopub_port": 33799,
  "stdin_port": 33829,
  "control_port": 36889,
  "hb_port": 39603,
  "ip": "127.0.0.1",
  "key": "7b1c9630-a5c87a6bd8f0822197ceff75",
  "transport": "tcp",
  "signature_scheme": "hmac-sha256",
  "kernel_name": ""
}
```

## jupyter kernel gateway

- ipynbがapi serverになる
- :warning: text/plainが付いたtextになりという場合がある。

## npviewer

- chrome extensionsとかにできない？

## zmq 

exampleはここにある

- [zguide/examples/Python at master · booksbyus/zguide](https://github.com/booksbyus/zguide/tree/master/examples/Python "zguide/examples/Python at master · booksbyus/zguide")

## dmypy mypy

- mypy/dmypy.py
- python-daemon
- unix domain sokectを使っている
- statusとかstart,stopの実装は参考になる
