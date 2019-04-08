## python GIL開放してbusyloop

- GIL開放してbusy loop
- pythonの中でCPUバウンドな処理書いても大丈夫？(asyncio)

## python asyncio timeoutを付ける方法

futureを共有してgiveup。必要？
- wait_for()で済むのでは？

## tiny loop

future

- Future.done()はblockしない
- Future.result()はblockする

queue

- Queue.put()はblockする
- Queue.get()もblockする
- (maxsizeがback pressureのように使われる)
- throttleは？


## python tornado gen.coroutineのデコレーターの意味が分かっていない

こういうやつ。

```python
from tornado import gen, web

class MainKernelHandler(APIHandler):
    @web.authenticated
    @gen.coroutine
    def get(self):
        km = self.kernel_manager
        kernels = yield gen.maybe_future(km.list_kernels())
        self.finish(json.dumps(kernels, default=date_default))
```

[maybe_future()](https://www.tornadoweb.org/en/stable/gen.html#tornado.gen.maybe_future)は知らないな。素直に `Any -> _asyncio.Future` 的なものか。

### tornadoのcoroutinesについて

まぁたぶんasync/awaitのyield版的なものだと思うけれど。

https://www.tornadoweb.org/en/stable/guide/coroutines.html

丁寧に読んでいくか。

```python
async def fetch_corountine(url):
    http_client = AsyncHTTPClient()
    response = await http_client.fetch(url)
    return response.body
```

まぁ普通のawaitable function。こちらを "native coroutines" とtornadoでは呼んでいるらしい。これに対して "decorated coroutines" だとか "yield-based corountines" という名前で古い方の記述方法のことを呼んでいるらしい。そこで出てくるのが `tornado.gen.corountine`

```python
from tornado import gen

@gen.corountine
def fetch_corountine(url):
    http_client = AsyncHTTPClient()
    response = yield http_client.fetch(url)
    return gen.Result(response.body)
```

これがどういう仕組みで動いているかと言うと、

```python
def run(self):
    future = self.gen.send(self.next)
    def callback():
        self.next = f.result()
        self.run()
    future.add_done_callback(callback)
```

これを呼ぶ方法は以下(awaitable functionの中で呼ぶ他)

```python
IOLoop.current().spawn_callback(coro)

IOLoop.current().run_sync(lambda: coro())
```

corountine(ここではtornadoの文脈での)の利用例

#### calling blocking function

run_in_executorを呼ぶ。

```python
async def call_blocking():
    await IOLoop.current().run_in_executor(None, blocking_func, args)
```

asyncioとほぼ一緒。

#### parallerism

multiを使う。

```python
from tornado.gen import multi

async def parallel_fetch(url1, url2):
    resp1, resp2 = await multi([http_client.fetch(url1), http_client.fetch(url2)])

async def parallel_fetch_dict(urls):
    resp1, resp2 = await multi({url: http_client.fetch(url) for url in urls})
```

dictも行ける以外はasyncio.gatherと変わらなそう。

"decorated corontine" の場合には、直接呼べる模様。

```python
@gen.coroutine
def parallel_fetch_decorated(url1, url2):
    resp1, resp2 = yield [http_client.fetch(url1), http_client.fetch(url2)]
```

#### interleaving

```python
from tornado.gen import convert_yielded

async def get(self):
    fetch_future = convert_yielded(self.fetch_next_chunk())
    while True:
        chunk = yield fetch_future
        if chunk is None:
            break
    	self.write(chunk)
        fetch_future = convert_yielded(self.fetch_next_chunk())
        yield self.flush()
```

これの意味はwaiting(yield)の前段階で何か処理をはさみたいみたいな意味か。具体的にはflushがやりたいのかな。

## jupyter io_pubで通常の通信のときに何が飛んでいるかを把握したかった

ふつうに `--debug` 付きでnotebookを起動するのが手軽そう。 

```
VENV/lib/python3.7/site-packages/notebook/services/kernels/kernelmanager.py:19: VisibleDeprecationWarning: zmq.eventloop.minitornado is deprecated in pyzmq 14.0 and will be removed.
    Install tornado itself to use zmq with the tornado IOLoop.
```

以下の様なエラーが出る。

```
[E 19:29:58.082 NotebookApp] Uncaught exception GET /notebooks/Untitled2.ipynb?kernel_name=python3 (127.0.0.1)
    HTTPServerRequest(protocol='http', host='127.0.0.1:8888', method='GET', uri='/notebooks/Untitled2.ipynb?kernel_name=python3', version='HTTP/1.1', remote_ip='127.0.0.1')
    Traceback (most recent call last):
      File "VENV/lib/python3.7/site-packages/tornado/web.py", line 1697, in _execute
        result = method(*self.path_args, **self.path_kwargs)
...
      File "VENV/lib/python3.7/site-packages/nbconvert/postprocessors/serve.py", line 19, in <module>
        class ProxyHandler(web.RequestHandler):
      File "VENV/lib/python3.7/site-packages/nbconvert/postprocessors/serve.py", line 21, in ProxyHandler
        @web.asynchronous
    AttributeError: module 'tornado.web' has no attribute 'asynchronous'
```

新しい環境で実行したら何か直った（後で調べる）

```console
$ pip freeze | grep -Po '^[^=]+' /tmp/after | tr "\n" "|"
$ pip freeze | grep -P '^(attrs|backcall|bleach|decorator|defusedxml|entrypoints|ipykernel|ipython|ipython-genutils|jedi|Jinja2|jsonschema|jupyter-client|jupyter-core|MarkupSafe|mistune|nbconvert|nbformat|notebook|pandocfilters|parso|pexpect|pickleshare|prometheus-client|prompt-toolkit|ptyprocess|Pygments|pyrsistent|python-dateutil|pyzmq|Send2Trash|six|terminado|testpath|tornado|traitlets|wcwidth|webencodings)$' | sort | tee after.txt
```

問題はnbconvertの模様(tracebackを丁寧に見れば分かる)。

```diff
--- /tmp/before.txt	2019-04-06 19:48:33.200172899 +0900
+++ /tmp/after	2019-04-06 19:39:29.361048505 +0900
@@ -1,17 +1,18 @@
-attrs==18.2.0
+attrs==19.1.0
 backcall==0.1.0
-bleach==2.1.4
-decorator==4.3.0
+bleach==3.1.0
+decorator==4.4.0
+defusedxml==0.5.0
 entrypoints==0.3
-ipykernel==4.8.2
-ipython==7.2.0
+ipykernel==5.1.0
+ipython==7.4.0
 ipython-genutils==0.2.0
-jedi==0.13.2
+jedi==0.13.3
 Jinja2==2.10
-jsonschema==2.6.0
-jupyter-client==5.2.3
+jsonschema==3.0.1
+jupyter-client==5.2.4
 jupyter-core==4.4.0
-MarkupSafe==1.0
-mistune==0.8.3
-nbconvert==5.3.1
+MarkupSafe==1.1.1
+mistune==0.8.4
+nbconvert==5.4.1
 nbformat==4.4.0
@@ -19,15 +20,16 @@
 pandocfilters==1.4.2
-parso==0.3.1
+parso==0.4.0
 pexpect==4.6.0
-pickleshare==0.7.4
-prometheus-client==0.3.1
+pickleshare==0.7.5
+prometheus-client==0.6.0
 prompt-toolkit==2.0.9
 ptyprocess==0.6.0
-Pygments==2.2.0
-python-dateutil==2.7.3
+Pygments==2.3.1
+pyrsistent==0.14.11
+python-dateutil==2.8.0
 pyzmq==18.0.1
 Send2Trash==1.5.0
-six==1.11.0
-terminado==0.8.1
-testpath==0.3.1
+six==1.12.0
+terminado==0.8.2
+testpath==0.4.2
 tornado==6.0.2
```

### log

以下のようなログがたくさん(debug log)

```
[D 19:34:05.468 NotebookApp] activity on 1a7ece4e-9fb7-434a-942b-022e0b4e3654: execute_input
[D 19:34:05.470 NotebookApp] activity on 1a7ece4e-9fb7-434a-942b-022e0b4e3654: stream
[D 19:34:05.474 NotebookApp] activity on 1a7ece4e-9fb7-434a-942b-022e0b4e3654: status
```

表示されているのはこの辺

/notebook/services/kernels/handlers.py

```python
class ZMQChannelsHandler(AuthenticatedZMQStreamHandler):
    def start_watching_activity(self, kernel_id):
        """Start watching IOPub messages on a kernel for activity.
        
        - update last_activity on every message
        - record execution_state from status messages
        """
        kernel = self._kernels[kernel_id]
        # add busy/activity markers:
        kernel.execution_state = 'starting'
        kernel.last_activity = utcnow()
        kernel._activity_stream = kernel.connect_iopub()
        session = Session(
            config=kernel.session.config,
            key=kernel.session.key,
        )

        def record_activity(msg_list):
            """Record an IOPub message arriving from a kernel"""
            self.last_kernel_activity = kernel.last_activity = utcnow()

            idents, fed_msg_list = session.feed_identities(msg_list)
            msg = session.deserialize(fed_msg_list)

            msg_type = msg['header']['msg_type']
            self.log.debug("activity on %s: %s", kernel_id, msg_type)
            if msg_type == 'status':
                kernel.execution_state = msg['content']['execution_state']

        kernel._activity_stream.on_recv(record_activity)
```

watching activityの意味を把握しないとということか。

そもそもこの start_watching_activity が呼ばれるのは、以下のメソッド。


```python
    @gen.coroutine
    def start_kernel(self, kernel_id=None, path=None, **kwargs):
        """Start a kernel for a session and return its kernel_id.

        Parameters
        ----------
        kernel_id : uuid
            The uuid to associate the new kernel with. If this
            is not None, this kernel will be persistent whenever it is
            requested.
        path : API path
            The API path (unicode, '/' delimited) for the cwd.
            Will be transformed to an OS path relative to root_dir.
        kernel_name : str
            The name identifying which kernel spec to launch. This is ignored if
            an existing kernel is returned, but it may be checked in the future.
        """
        if kernel_id is None:
            if path is not None:
                kwargs['cwd'] = self.cwd_for_path(path)
            kernel_id = yield gen.maybe_future(
                super(MappingKernelManager, self).start_kernel(**kwargs)
            )
            self._kernel_connections[kernel_id] = 0
            self.start_watching_activity(kernel_id)
            self.log.info("Kernel started: %s" % kernel_id)
            self.log.debug("Kernel args: %r" % kwargs)
            # register callback for failed auto-restart
            self.add_restart_callback(kernel_id,
                lambda : self._handle_kernel_died(kernel_id),
                'dead',
            )
        else:
            self._check_kernel_id(kernel_id)
            self.log.info("Using existing kernel: %s" % kernel_id)

        # Initialize culling if not already
        if not self._initialized_culler:
            self.initialize_culler()

        # py2-compat
        raise gen.Return(kernel_id)
```

memo

- start_kernel() の結果kernel_idが手に入る。らしい。
- kernel_connectionsの中にmapが存在
- start_kernel() 時に start_watching_activity() をする。
- (add_restart_callback() が呼ばれるタイミングはよくわからない)
- kernel_id が存在している場合はそれを使う。

ほしかったのはstart_watching_activity()かもしれない？
たぶんこれらはMultiKernelManagerの仕組みなような気がしてきた。

hmm

- write_message()
- session.msg()
- (iopub って flush() しないとダメ？)
- (_send_status_message() は write_message() を呼ぶ)

そもそも使われ方がわからない？

## jupyterでのzmqとの通信

websocketとしてzmqを公開している部分がすごい。

/notebook/base/zmqhandlers.py

```
notebook.base.zmqhandlers:ZMQStreamHandler <- notebook.base.zmqhandlers:WebSocketMixin <- tornado.websocket:WebSocketHandler <- tornado.web:RequestHandler <- builtins:object
    [method] _on_zmq_reply(self, stream, msg_list)
        [method] _reserialize_reply(self, msg_or_list, channel=None)

notebook.base.zmqhandlers:WebSocketMixin <- builtins:object
    [method] check_origin(self, origin=None)
    [method] clear_cookie(self, *args, **kwargs)
    [method] on_pong(self, data)
    [method] open(self, *args, **kwargs)
        [property] ping_interval
        [method] send_ping(self)
            [property] ping_timeout
            [property] ping_interval
```

ここの `_on_zmq_reply()` は `notebook.services.kernels.handlers:ZMQChannelsHandler` の `open()` で使われている。内部的にはserializeされて欲しいので必要なら `_reserialize_reply()` をする。


## jupyter kernelとの接続

なにか通信のcaptureをする術はないものか？

💭 notebookのやっている部分を見るか

- notebook/notebookapp.py
- tornadoを理解しないとかも？

```
        handlers.extend(load_handlers('notebook.notebook.handlers'))
```

- 根本はMultiKernelManager?

### jupyter-notebookを立ち上げる

既に立ち上がっているサーバーを見るコマンドもある。

```
$ jupyter-notebook list
Currently running servers:
```

memo: 証明書で認証もできるらしい(client-ca, certfile, keyfile)

```
$ jupyter-notebook
...
OSError: [Errno 99] Cannot assign requested address
$ jupyter-notebook --ip=127.0.0.1
```

たぶんこの辺を理解すると良い。

/notebook/services/kernels/handlers.py

```python
class ZMQChannelsHandler(AuthenticatedZMQStreamHandler):

    (r"/api/kernels/%s/channels" % _kernel_id_regex, ZMQChannelsHandler),
```

- io_pubにrate limitが存在している
- `create_stream()` でstreamを作る

```
notebook.services.kernels.handlers:ZMQChannelsHandler <- notebook.base.zmqhandlers:AuthenticatedZMQStreamHandler <- notebook.base.zmqhandlers:ZMQStreamHandler <- notebook.base.zmqhandlers:WebSocketMixin <- tornado.websocket:WebSocketHandler <- notebook.base.handlers:IPythonHandler <- notebook.base.handlers:AuthenticatedHandler <- tornado.web:RequestHandler <- builtins:object
    [method, OVERRIDE] __repr__(self)
    [method, OVERRIDE] get(self, kernel_id)
    [method, OVERRIDE] initialize(self)
    [method, OVERRIDE] on_close(self)
        [method] on_kernel_restarted(self)
            [method] _send_status_message(self, status)
        [method] on_restart_failed(self)
            [method] _send_status_message(self, status)
    [method, OVERRIDE] on_message(self, msg)
    [method, OVERRIDE] open(self, kernel_id)
        [method] on_kernel_restarted(self)
            [method] _send_status_message(self, status)
        [method] on_restart_failed(self)
            [method] _send_status_message(self, status)
        [method, OVERRIDE] _on_zmq_reply(self, stream, msg_list)
            [property] rate_limit_window
            [property] iopub_msg_rate_limit
            [property] iopub_data_rate_limit
        [method] create_stream(self)
        [method, OVERRIDE] close(self)
    [method, OVERRIDE] pre_get(self)
        [method] request_kernel_info(self)
            [method] _handle_kernel_info_reply(self, msg)
                [method] _finish_kernel_info(self, info)
            [method] _finish_kernel_info(self, info)
        [method] _register_session(self)
        [property] kernel_info_timeout

notebook.base.zmqhandlers:AuthenticatedZMQStreamHandler <- notebook.base.zmqhandlers:ZMQStreamHandler <- notebook.base.zmqhandlers:WebSocketMixin <- tornado.websocket:WebSocketHandler <- notebook.base.handlers:IPythonHandler <- notebook.base.handlers:AuthenticatedHandler <- tornado.web:RequestHandler <- builtins:object
    [method, OVERRIDE] get(self, *args, **kwargs)
        [method] pre_get(self)
    [method, OVERRIDE] get_compression_options(self)
    [method, OVERRIDE] initialize(self)
    [method, OVERRIDE] set_default_headers(self)

notebook.base.zmqhandlers:ZMQStreamHandler <- notebook.base.zmqhandlers:WebSocketMixin <- tornado.websocket:WebSocketHandler <- tornado.web:RequestHandler <- builtins:object
    [method] _on_zmq_reply(self, stream, msg_list)
        [method] _reserialize_reply(self, msg_or_list, channel=None)

notebook.base.zmqhandlers:WebSocketMixin <- builtins:object
    [method] check_origin(self, origin=None)
    [method] clear_cookie(self, *args, **kwargs)
    [method] on_pong(self, data)
    [method] open(self, *args, **kwargs)
        [property] ping_interval
        [method] send_ping(self)
            [property] ping_timeout
            [property] ping_interval

tornado.websocket:WebSocketHandler <- tornado.web:RequestHandler <- builtins:object
    [method, OVERRIDE] __init__(self, application: tornado.web.Application, request: tornado.httputil.HTTPServerRequest, **kwargs: Any) -> None
    [method] _detach_stream(self) -> tornado.iostream.IOStream
    [method] close(self, code: int = None, reason: str = None) -> None
    [method, OVERRIDE] get(self, *args: Any, **kwargs: Any) -> None
        [method] get_websocket_protocol(self) -> Union[ForwardRef('WebSocketProtocol'), NoneType]
            [property] ping_interval
            [property] ping_timeout
            [property] max_message_size
            [method] get_compression_options(self) -> Union[Dict[str, Any], NoneType]
        [method] check_origin(self, origin: str) -> bool
    [method] on_message(self, message: Union[str, bytes]) -> Union[Awaitable[NoneType], NoneType]
    [method] on_ping(self, data: bytes) -> None
    [method] on_pong(self, data: bytes) -> None
    [method] on_ws_connection_close(self, close_code: int = None, close_reason: str = None) -> None
        [method, OVERRIDE] on_connection_close(self) -> None
            [method] on_close(self) -> None
            [method, OVERRIDE] _break_cycles(self) -> None
    [method] open(self, *args: str, **kwargs: str) -> Union[Awaitable[NoneType], NoneType]
    [method] ping(self, data: Union[str, bytes] = b'') -> None
    [method] select_subprotocol(self, subprotocols: List[str]) -> Union[str, NoneType]
    [property] selected_subprotocol
    [method, OVERRIDE] send_error(self, *args: Any, **kwargs: Any) -> None
    [method] set_nodelay(self, value: bool) -> None
    [method] write_message(self, message: Union[bytes, str, Dict[str, Any]], binary: bool = False) -> 'Future[None]'

notebook.base.handlers:IPythonHandler <- notebook.base.handlers:AuthenticatedHandler <- tornado.web:RequestHandler <- builtins:object
    [property] base_url
    [method] check_origin(self, origin_to_satisfy_tornado='')
        [property] allow_origin
        [property] allow_origin_pat
        [property] log
    [method, OVERRIDE] check_xsrf_cookie(self)
    [property] config
    [property] config_manager
    [property] contents_js_source
    [property] contents_manager
    [property] default_url
    [method] get_json_body(self)
        [property] log
    [property] ignore_minified_js
    [property] jinja_template_vars
    [property] kernel_manager
    [property] kernel_spec_manager
    [property] mathjax_config
    [property] mathjax_url
    [method, OVERRIDE] prepare(self)
        [method] check_host(self)
            [property] log
    [property] session_manager
    [method] set_attachment_header(self, filename)
    [method, OVERRIDE] set_default_headers(self)
        [property] allow_origin
        [property] allow_credentials
        [property] allow_origin_pat
        [method] get_origin(self)
    [property] terminal_manager
    [property] version_hash
    [method, OVERRIDE] write_error(self, status_code, **kwargs)
        [method] render_template(self, name, **ns)
            [property] template_namespace
            [method] get_template(self, name)
    [property] ws_url

notebook.base.handlers:AuthenticatedHandler <- tornado.web:RequestHandler <- builtins:object
    [method] clear_login_cookie(self)
        [property] cookie_name
        [method] force_clear_cookie(self, name, path='/', domain=None)
    [method, OVERRIDE] get_current_user(self)
        [property] login_handler
    [property] logged_in
    [property] login_available
    [property] one_time_token
    [method, OVERRIDE] set_default_headers(self)
        [property] content_security_policy
    [method] skip_check_origin(self)
        [property] login_handler
    [property] token
    [property] token_authenticated

tornado.web:RequestHandler <- builtins:object
    [method, OVERRIDE] __init__(self, application: 'Application', request: tornado.httputil.HTTPServerRequest, **kwargs: Any) -> None
        [method] clear(self) -> None
            [method] set_default_headers(self) -> None
        [method] on_connection_close(self) -> None
        [method] initialize(self) -> None
        [method] _ui_method(self, method: Callable[..., str]) -> Callable[..., str]
    [method] _execute(self, transforms: List[ForwardRef('OutputTransform')], *args: bytes, **kwargs: bytes) -> None
        [method] prepare(self) -> Union[Awaitable[NoneType], NoneType]
        [method] decode_argument(self, value: bytes, name: str = None) -> str
        [method] check_xsrf_cookie(self) -> None
            [method] _decode_xsrf_token(self, cookie: str) -> Tuple[Union[int, NoneType], Union[bytes, NoneType], Union[float, NoneType]]
            [method] _get_raw_xsrf_token(self) -> Tuple[Union[int, NoneType], bytes, float]
                [method] get_cookie(self, name: str, default: str = None) -> Union[str, NoneType]
                [method] _decode_xsrf_token(self, cookie: str) -> Tuple[Union[int, NoneType], Union[bytes, NoneType], Union[float, NoneType]]
            [method] get_argument(self, name: str, default: Union[NoneType, str, tornado.web._ArgDefaultMarker] = <tornado.web._ArgDefaultMarker object at 0x7f612eefa908>, strip: bool = True) -> Union[str, NoneType]
                [method] _get_argument(self, name: str, default: Union[NoneType, str, tornado.web._ArgDefaultMarker], source: Dict[str, List[bytes]], strip: bool = True) -> Union[str, NoneType]
                    [method] _get_arguments(self, name: str, source: Dict[str, List[bytes]], strip: bool = True) -> List[str]
                        [method] decode_argument(self, value: bytes, name: str = None) -> str
        [method] finish(self, chunk: Union[str, bytes, dict] = None) -> 'Future[None]'
            [method] flush(self, include_footers: bool = False) -> 'Future[None]'
                [method] add_header(self, name: str, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> None
                    [method] _convert_header_value(self, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> str
            [method] _log(self) -> None
            [method] on_finish(self) -> None
            [method] _break_cycles(self) -> None
            [method] write(self, chunk: Union[str, bytes, dict]) -> None
                [method] set_header(self, name: str, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> None
                    [method] _convert_header_value(self, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> str
            [method] set_etag_header(self) -> None
                [method] compute_etag(self) -> Union[str, NoneType]
                [method] set_header(self, name: str, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> None
                    [method] _convert_header_value(self, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> str
            [method] check_etag_header(self) -> bool
            [method] _clear_headers_for_304(self) -> None
                [method] clear_header(self, name: str) -> None
            [method] set_status(self, status_code: int, reason: str = None) -> None
            [method] set_header(self, name: str, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> None
                [method] _convert_header_value(self, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> str
        [method] _handle_request_exception(self, e: BaseException) -> None
            [method] log_exception(self, typ: 'Optional[Type[BaseException]]', value: Union[BaseException, NoneType], tb: Union[traceback, NoneType]) -> None
                [method] _request_summary(self) -> str
            [method] send_error(self, status_code: int = 500, **kwargs: Any) -> None
                [method] clear(self) -> None
                    [method] set_default_headers(self) -> None
                [method] set_status(self, status_code: int, reason: str = None) -> None
                [method] write_error(self, status_code: int, **kwargs: Any) -> None
                    [method] set_header(self, name: str, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> None
                        [method] _convert_header_value(self, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> str
                    [method] finish(self, chunk: Union[str, bytes, dict] = None) -> 'Future[None]'
                        [method] flush(self, include_footers: bool = False) -> 'Future[None]'
                            [method] add_header(self, name: str, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> None
                                [method] _convert_header_value(self, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> str
                        [method] _log(self) -> None
                        [method] on_finish(self) -> None
                        [method] _break_cycles(self) -> None
                        [method] write(self, chunk: Union[str, bytes, dict]) -> None
                            [method] set_header(self, name: str, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> None
                                [method] _convert_header_value(self, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> str
                        [method] set_etag_header(self) -> None
                            [method] compute_etag(self) -> Union[str, NoneType]
                            [method] set_header(self, name: str, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> None
                                [method] _convert_header_value(self, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> str
                        [method] check_etag_header(self) -> bool
                        [method] _clear_headers_for_304(self) -> None
                            [method] clear_header(self, name: str) -> None
                        [method] set_status(self, status_code: int, reason: str = None) -> None
                        [method] set_header(self, name: str, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> None
                            [method] _convert_header_value(self, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> str
                    [property] settings
                    [method] write(self, chunk: Union[str, bytes, dict]) -> None
                        [method] set_header(self, name: str, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> None
                            [method] _convert_header_value(self, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> str
                [method] finish(self, chunk: Union[str, bytes, dict] = None) -> 'Future[None]'
                    [method] flush(self, include_footers: bool = False) -> 'Future[None]'
                        [method] add_header(self, name: str, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> None
                            [method] _convert_header_value(self, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> str
                    [method] _log(self) -> None
                    [method] on_finish(self) -> None
                    [method] _break_cycles(self) -> None
                    [method] write(self, chunk: Union[str, bytes, dict]) -> None
                        [method] set_header(self, name: str, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> None
                            [method] _convert_header_value(self, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> str
                    [method] set_etag_header(self) -> None
                        [method] compute_etag(self) -> Union[str, NoneType]
                        [method] set_header(self, name: str, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> None
                            [method] _convert_header_value(self, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> str
                    [method] check_etag_header(self) -> bool
                    [method] _clear_headers_for_304(self) -> None
                        [method] clear_header(self, name: str) -> None
                    [method] set_status(self, status_code: int, reason: str = None) -> None
                    [method] set_header(self, name: str, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> None
                        [method] _convert_header_value(self, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> str
            [method] finish(self, chunk: Union[str, bytes, dict] = None) -> 'Future[None]'
                [method] flush(self, include_footers: bool = False) -> 'Future[None]'
                    [method] add_header(self, name: str, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> None
                        [method] _convert_header_value(self, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> str
                [method] _log(self) -> None
                [method] on_finish(self) -> None
                [method] _break_cycles(self) -> None
                [method] write(self, chunk: Union[str, bytes, dict]) -> None
                    [method] set_header(self, name: str, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> None
                        [method] _convert_header_value(self, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> str
                [method] set_etag_header(self) -> None
                    [method] compute_etag(self) -> Union[str, NoneType]
                    [method] set_header(self, name: str, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> None
                        [method] _convert_header_value(self, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> str
                [method] check_etag_header(self) -> bool
                [method] _clear_headers_for_304(self) -> None
                    [method] clear_header(self, name: str) -> None
                [method] set_status(self, status_code: int, reason: str = None) -> None
                [method] set_header(self, name: str, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> None
                    [method] _convert_header_value(self, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> str
    [method] _initialize(self) -> None
    [method] _ui_module(self, name: str, module: Type[ForwardRef('UIModule')]) -> Callable[..., str]
    [method] _unimplemented_method(self, *args: str, **kwargs: str) -> None
    [method] clear_all_cookies(self, path: str = '/', domain: str = None) -> None
        [method] clear_cookie(self, name: str, path: str = '/', domain: str = None) -> None
            [method] set_cookie(self, name: str, value: Union[str, bytes], domain: str = None, expires: Union[float, Tuple, datetime.datetime] = None, path: str = '/', expires_days: int = None, **kwargs: Any) -> None
    [property] cookies
    [method] data_received(self, chunk: bytes) -> Union[Awaitable[NoneType], NoneType]
    [method] delete(self, *args: str, **kwargs: str) -> None
    [method] detach(self) -> tornado.iostream.IOStream
    [method] get(self, *args: str, **kwargs: str) -> None
    [method] get_arguments(self, name: str, strip: bool = True) -> List[str]
        [method] _get_arguments(self, name: str, source: Dict[str, List[bytes]], strip: bool = True) -> List[str]
            [method] decode_argument(self, value: bytes, name: str = None) -> str
    [method] get_body_argument(self, name: str, default: Union[NoneType, str, tornado.web._ArgDefaultMarker] = <tornado.web._ArgDefaultMarker object at 0x7f612eefa908>, strip: bool = True) -> Union[str, NoneType]
        [method] _get_argument(self, name: str, default: Union[NoneType, str, tornado.web._ArgDefaultMarker], source: Dict[str, List[bytes]], strip: bool = True) -> Union[str, NoneType]
            [method] _get_arguments(self, name: str, source: Dict[str, List[bytes]], strip: bool = True) -> List[str]
                [method] decode_argument(self, value: bytes, name: str = None) -> str
    [method] get_body_arguments(self, name: str, strip: bool = True) -> List[str]
        [method] _get_arguments(self, name: str, source: Dict[str, List[bytes]], strip: bool = True) -> List[str]
            [method] decode_argument(self, value: bytes, name: str = None) -> str
    [method] get_browser_locale(self, default: str = 'en_US') -> tornado.locale.Locale
    [method] get_current_user(self) -> Any
    [method] get_login_url(self) -> str
        [method] require_setting(self, name: str, feature: str = 'this feature') -> None
    [method] get_query_argument(self, name: str, default: Union[NoneType, str, tornado.web._ArgDefaultMarker] = <tornado.web._ArgDefaultMarker object at 0x7f612eefa908>, strip: bool = True) -> Union[str, NoneType]
        [method] _get_argument(self, name: str, default: Union[NoneType, str, tornado.web._ArgDefaultMarker], source: Dict[str, List[bytes]], strip: bool = True) -> Union[str, NoneType]
            [method] _get_arguments(self, name: str, source: Dict[str, List[bytes]], strip: bool = True) -> List[str]
                [method] decode_argument(self, value: bytes, name: str = None) -> str
    [method] get_query_arguments(self, name: str, strip: bool = True) -> List[str]
        [method] _get_arguments(self, name: str, source: Dict[str, List[bytes]], strip: bool = True) -> List[str]
            [method] decode_argument(self, value: bytes, name: str = None) -> str
    [method] get_secure_cookie(self, name: str, value: str = None, max_age_days: int = 31, min_version: int = None) -> Union[bytes, NoneType]
        [method] require_setting(self, name: str, feature: str = 'this feature') -> None
        [method] get_cookie(self, name: str, default: str = None) -> Union[str, NoneType]
    [method] get_secure_cookie_key_version(self, name: str, value: str = None) -> Union[int, NoneType]
        [method] require_setting(self, name: str, feature: str = 'this feature') -> None
        [method] get_cookie(self, name: str, default: str = None) -> Union[str, NoneType]
    [method] get_status(self) -> int
    [method] get_user_locale(self) -> Union[tornado.locale.Locale, NoneType]
    [method] head(self, *args: str, **kwargs: str) -> None
    [method] options(self, *args: str, **kwargs: str) -> None
    [method] patch(self, *args: str, **kwargs: str) -> None
    [method] post(self, *args: str, **kwargs: str) -> None
    [method] put(self, *args: str, **kwargs: str) -> None
    [method] redirect(self, url: str, permanent: bool = False, status: int = None) -> None
        [method] set_status(self, status_code: int, reason: str = None) -> None
        [method] set_header(self, name: str, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> None
            [method] _convert_header_value(self, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> str
        [method] finish(self, chunk: Union[str, bytes, dict] = None) -> 'Future[None]'
            [method] flush(self, include_footers: bool = False) -> 'Future[None]'
                [method] add_header(self, name: str, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> None
                    [method] _convert_header_value(self, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> str
            [method] _log(self) -> None
            [method] on_finish(self) -> None
            [method] _break_cycles(self) -> None
            [method] write(self, chunk: Union[str, bytes, dict]) -> None
                [method] set_header(self, name: str, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> None
                    [method] _convert_header_value(self, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> str
            [method] set_etag_header(self) -> None
                [method] compute_etag(self) -> Union[str, NoneType]
                [method] set_header(self, name: str, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> None
                    [method] _convert_header_value(self, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> str
            [method] check_etag_header(self) -> bool
            [method] _clear_headers_for_304(self) -> None
                [method] clear_header(self, name: str) -> None
            [method] set_status(self, status_code: int, reason: str = None) -> None
            [method] set_header(self, name: str, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> None
                [method] _convert_header_value(self, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> str
    [method] render(self, template_name: str, **kwargs: Any) -> 'Future[None]'
        [method] render_string(self, template_name: str, **kwargs: Any) -> bytes
            [method] get_template_path(self) -> Union[str, NoneType]
            [method] get_template_namespace(self) -> Dict[str, Any]
                [property] current_user
                [property] locale
                [method] static_url(self, path: str, include_host: bool = None, **kwargs: Any) -> str
                    [method] require_setting(self, name: str, feature: str = 'this feature') -> None
                    [property] settings
                [method] xsrf_form_html(self) -> str
                    [property] xsrf_token
                [method] reverse_url(self, name: str, *args: Any) -> str
            [method] create_template_loader(self, template_path: str) -> tornado.template.BaseLoader
        [method] finish(self, chunk: Union[str, bytes, dict] = None) -> 'Future[None]'
            [method] flush(self, include_footers: bool = False) -> 'Future[None]'
                [method] add_header(self, name: str, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> None
                    [method] _convert_header_value(self, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> str
            [method] _log(self) -> None
            [method] on_finish(self) -> None
            [method] _break_cycles(self) -> None
            [method] write(self, chunk: Union[str, bytes, dict]) -> None
                [method] set_header(self, name: str, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> None
                    [method] _convert_header_value(self, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> str
            [method] set_etag_header(self) -> None
                [method] compute_etag(self) -> Union[str, NoneType]
                [method] set_header(self, name: str, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> None
                    [method] _convert_header_value(self, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> str
            [method] check_etag_header(self) -> bool
            [method] _clear_headers_for_304(self) -> None
                [method] clear_header(self, name: str) -> None
            [method] set_status(self, status_code: int, reason: str = None) -> None
            [method] set_header(self, name: str, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> None
                [method] _convert_header_value(self, value: Union[bytes, str, int, numbers.Integral, datetime.datetime]) -> str
        [method] render_linked_js(self, js_files: Iterable[str]) -> str
            [method] static_url(self, path: str, include_host: bool = None, **kwargs: Any) -> str
                [method] require_setting(self, name: str, feature: str = 'this feature') -> None
                [property] settings
        [method] render_embed_js(self, js_embed: Iterable[bytes]) -> bytes
        [method] render_linked_css(self, css_files: Iterable[str]) -> str
            [method] static_url(self, path: str, include_host: bool = None, **kwargs: Any) -> str
                [method] require_setting(self, name: str, feature: str = 'this feature') -> None
                [property] settings
        [method] render_embed_css(self, css_embed: Iterable[bytes]) -> bytes
    [method] set_secure_cookie(self, name: str, value: Union[str, bytes], expires_days: int = 30, version: int = None, **kwargs: Any) -> None
        [method] set_cookie(self, name: str, value: Union[str, bytes], domain: str = None, expires: Union[float, Tuple, datetime.datetime] = None, path: str = '/', expires_days: int = None, **kwargs: Any) -> None
        [method] create_signed_value(self, name: str, value: Union[str, bytes], version: int = None) -> bytes
            [method] require_setting(self, name: str, feature: str = 'this feature') -> None
```

## harを取り出す

```console
# 形状を取り出す
$ dictknife -i json shape --with-type <target>

# URLを取り出す
$ jqfpy 'get("log/entries[]/request/url")' ${TARGET} --squash --raw

# url,method,statusを取り出す
jqfpy '[h.pick("request/url", "request/method", "response/status", d=d) for d in get("log/entries[]")]' ${TARGET}
```


## python tornado

そういえばtornado知らないな。

- https://github.com/tornadoweb/tornado/tree/master/demos

