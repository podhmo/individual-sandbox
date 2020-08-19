from __future__ import annotations

# https://github.com/grpc/grpc/blob/master/examples/protos/keyvaluestore.proto


class KeyValueStore:
    def getValues(self, stream: Request) -> Response:
        ...


class Request:
    key: str


class Response:
    value: str
