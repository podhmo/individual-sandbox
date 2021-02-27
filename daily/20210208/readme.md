## python socket

- `socket.IPPROTO_IP`って何者？
- client, serverどっちがどっち？
- TCP?,UDP?
- なにを送受信しているの？

この辺だいぶ忘れてしまっているな。

### 追記

- https://docs.python.org/ja/3/library/socket.html#socket.socket
- https://docs.python.org/ja/3/howto/sockets.html
- https://docs.python.org/ja/3/library/socketserver.html#socketserver-tcpserver-example
- https://qiita.com/__init__/items/5c89fa5b37b8c5ed32a4
- https://qiita.com/Michinosuke/items/0778a5344bdf81488114
- socketのbind, connect

defaultのsocketの作成は `socket.socket(AF_INET, SOCK_STREAM, IPPROTO_IP)`

```
>>> [(k,v) for k,v in socket.__dict__.items() if k.startswith("IPPROTO_")]
[('IPPROTO_IP', 0), ('IPPROTO_HOPOPTS', 0), ('IPPROTO_ICMP', 1), ('IPPROTO_IGMP', 2), ('IPPROTO_GGP', 3), ('IPPROTO_IPV4', 4), ('IPPROTO_IPV6', 41), ('IPPROTO_IPIP', 4), ('IPPROTO_TCP', 6), ('IPPROTO_EGP', 8), ('IPPROTO_PUP', 12), ('IPPROTO_UDP', 17), ('IPPROTO_IDP', 22), ('IPPROTO_HELLO', 63), ('IPPROTO_ND', 77), ('IPPROTO_TP', 29), ('IPPROTO_ROUTING', 43), ('IPPROTO_FRAGMENT', 44), ('IPPROTO_RSVP', 46), ('IPPROTO_GRE', 47), ('IPPROTO_ESP', 50), ('IPPROTO_AH', 51), ('IPPROTO_ICMPV6', 58), ('IPPROTO_NONE', 59), ('IPPROTO_DSTOPTS', 60), ('IPPROTO_XTP', 36), ('IPPROTO_EON', 80), ('IPPROTO_PIM', 103), ('IPPROTO_IPCOMP', 108), ('IPPROTO_SCTP', 132), ('IPPROTO_RAW', 255), ('IPPROTO_MAX', 256)]
```

### 通常の流れ

socket -> bind -> listen -> accept -> recv/send -> close

- bindでip,portを割り当てる
- listenで接続待ち状態にする
- acceptで通信を待ち受ける

server

```
socket()
bind()
listen()
```

### 雑多なメモ

```python
s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.bind((socket.gethostname(), 6000))  # IPとポート番号を指定します
ip = socket.gethostbyname(socket.gethostname())
print("Server IP : " + ip)
s.listen(5)
```

```python
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM, socket.IPPROTO_TCP) as sock:
        # マルチキャストJOIN
        sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        sock.setsockopt(
            socket.IPPROTO_IP,
            socket.IP_ADD_MEMBERSHIP,
            socket.inet_aton(ip) + socket.inet_aton("0.0.0.0"),
        )
        rfds = [sock]
        # 受信
        while is_recv:
            try:
                # 0.5秒ごとにselectタイムアウトで is_recvフラグチェックの機会を与える
                r, _, _ = select.select(rfds, [], [], 0.5)
                for rs in r:
                    data, addr = sock.recvfrom(4096)
                    print("received from={}, len={}".format(addr, len(data)))
            except Exception as ex:
                # print(ex)
                print("Exception: time={}, desc={}".format(datetime.datetime.now(), ex))
                s.close()
            finally:
                time.sleep(0.1)

        print("recv_func leaves.")
```
