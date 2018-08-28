```console
$ make serve
python 00serve.py --connection-name x.json
[MyKernelApp] Starting kernel 'python3'
[MyKernelApp] Connection file: x.json
[MyKernelApp] To connect a client: --existing x.json
```

client

```console
$ make client
python 00client.py --connection-name x.json
hello: 1
1
$ make client
python 00client.py --connection-name x.json
hello: 2
4
$ make client
python 00client.py --connection-name x.json
hello: 3
9
```
