## python rpc

- rpc.py
- json-rpc proto

hmmなるほどなー。前者はJRON-RPC 2.0とかには従っていない。
代わりに以下が手に入る

- openapi docでのschema
- 特別なことをしなくてもログで見える。

## interface

- repl
- cli
- web API

JSON-RPCで痛感したけれど。replやcliの場合には直接データを送れないところに違いがある。
cliで直接データを常に書くことを要求するのは厳しい。一方で、web APIはJSONをそのまま送れる。

