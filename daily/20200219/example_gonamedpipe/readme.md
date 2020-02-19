# goでもpipeによるコミュニケーションをやってみたい

## 00 namedpipe

問題は `net.Conn` のインターフェイスの上に載せたいということ。

## 01 stdin/stdout tcp

- https://stackoverflow.com/questions/42435064/tcp-send-stdout-to-client-and-read-stdin-from-client
