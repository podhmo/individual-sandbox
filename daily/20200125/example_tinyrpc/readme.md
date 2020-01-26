# tinyrpc

まだこれを使い続ける夢を諦めていない。

```console
$ pyinspect inspect tinyrpc.protocols.jsonrpc:JSONRPCProtocol
tinyrpc.protocols.jsonrpc:JSONRPCProtocol <- tinyrpc.protocols:RPCBatchProtocol <- tinyrpc.protocols:RPCProtocol <- abc:ABC <- builtins:object
    [method, OVERRIDE] __init__(self, *args, **kwargs) -> None
    [method] _caller(self, method: Callable, args: List[Any], kwargs: Dict[str, Any]) -> Any
    [method, OVERRIDE] create_batch_request(self, requests: Union[ForwardRef('JSONRPCRequest'), List[ForwardRef('JSONRPCRequest')]] = None) -> 'JSONRPCBatchRequest'
    [method, OVERRIDE] create_request(self, method: str, args: List[Any] = None, kwargs: Dict[str, Any] = None, one_way: bool = False) -> 'JSONRPCRequest'
        [method] request_factory(self) -> 'JSONRPCRequest'
        [method] _get_unique_id(self) -> int
    [method, OVERRIDE] parse_reply(self, data: bytes) -> Union[ForwardRef('JSONRPCSuccessResponse'), ForwardRef('JSONRPCErrorResponse')]
    [method, OVERRIDE] parse_request(self, data: bytes) -> Union[ForwardRef('JSONRPCRequest'), ForwardRef('JSONRPCBatchRequest')]
        [method] _parse_subrequest(self, req)
            [method] request_factory(self) -> 'JSONRPCRequest'
    [method, OVERRIDE] raise_error(self, error: Union[ForwardRef('JSONRPCErrorResponse'), Dict[str, Any]]) -> 'JSONRPCError'

tinyrpc.protocols:RPCBatchProtocol <- tinyrpc.protocols:RPCProtocol <- abc:ABC <- builtins:object
    [method] create_batch_request(self, requests: List[ForwardRef('RPCRequest')] = None) -> 'RPCBatchRequest'

tinyrpc.protocols:RPCProtocol <- abc:ABC <- builtins:object
    [method] create_request(self, method: str, args: List[Any] = None, kwargs: Dict[str, Any] = None, one_way: bool = False) -> 'RPCRequest'
    [method] parse_reply(self, data: bytes) -> 'RPCResponse'
    [method] parse_request(self, data: bytes) -> 'RPCRequest'
    [method] raise_error(self, error: 'RPCErrorResponse') -> tinyrpc.exc.RPCError

abc:ABC <- builtins:object
```
