## pstree

追記: 初手の確認には `ps -j --forest` が手軽そう

親側

```
pstree -A -p -s -h $(cat x.pid)
```

子側

```
pstree -A -p -h $(cat x.pid)
```

スレッドを無視して表示した方がわかりやすいかも (-T)
ascii modeの方が共有はしやすい？ (-A)
pidものこっていた方がうれしい (-p)

```console
$ pstree -T -p -A -s $(cat x.pid)
systemd(1)---gdm(461)---gdm-session-wor(1469)---gdm-wayland-ses(1488)---gnome-session-b(1492)+

$ pstree -T -p -A $(cat x.pid)
go(26890)---main(26947)
```

### psでもできる

ちなみにpsにforest option (--forest) があれば十分だったりする?

```console
$ ps --forest
  PID TTY          TIME CMD
 9349 pts/2    00:00:00 bash
26890 pts/2    00:00:00  \_ go
26947 pts/2    00:00:00  |   \_ main
28879 pts/2    00:00:00  \_ ps
```


hmm

```console
$ ps axfo stat,euid,ruid,tty,tpgid,sess,pgrp,ppid,pid,pcpu,comm | ioknife rest | grep -P "26890|26947|29260"

STAT  EUID  RUID TT       TPGID  SESS  PGRP  PPID   PID %CPU COMMAND
Sl    1000  1000 pts/2    29271  9349 26890  9349 26890  0.0      |       |   |   |   \_ go
Sl    1000  1000 pts/2    29271  9349 26890 26890 26947  0.0      |       |   |   |   |   \_ 
```

hmm

- TPGID // tty process group id
- PGRP,PGID // PGIDのalias process group ID or, equivalently, the process ID of the process group leader.  (alias pgrp).
- PPID // parent process ID

```
$ ps -o pid,ppid,pgid,cmd --forest
  PID  PPID  PGID CMD
 9349  4295  9349 /bin/bash --noediting -i
26890  9349 26890  \_ go run main.go
26947 26890 26890  |   \_ /tmp/go-build432369087/b001/exe/main
30478  9349 30478  \_ ps -o pid,ppid,pgid,cmd --forest

$ pstree -A -T -p -g $(cat x.pid)
go(26890,26890)---main(26947,26890)
```

## ioknife serverとclientを動かすMakefileを手軽に書きたいな

```
default:
	ioknife too --cmd "make client" --cmd "make server"

client
	do something

server:
	do something
```

問題は go runで動かしたもののprocessの形っぽい。実装自体は立ち上げたprocessの中でbuildして実行なのだけれどその時subprocessをたちあげているっぽい。

```console
$ go run main.go&  # launch server
[1] 26890
$ echo $! > x.pid
$ cat x.pid
26890
$ ps -j
  PID  PGID   SID TTY          TIME CMD
 9349  9349  9349 pts/2    00:00:00 bash
26890 26890  9349 pts/2    00:00:00 go
26947 26890  9349 pts/2    00:00:00 main
27401 27401  9349 pts/2    00:00:00 ps

$ ps -o pid,pgid,time,cmd --forest
  PID  PGID     TIME CMD
 9349  9349 00:00:00 /bin/bash --noediting -i
26890 26890 00:00:00  \_ go run main.go
26947 26890 00:00:00  |   \_ /tmp/go-build432369087/b001/exe/main
30689 30689 00:00:00  \_ ps -o pid,pgid,time,cmd --forest

$ pstree -T -p -g -A $(cat x.pid)
go(26890,26890)---main(26947,26890)

# pkillを使うのが手軽そう
$ pkill -TERM -g 26890
```

- https://stackoverflow.com/questions/392022/whats-the-best-way-to-send-a-signal-to-all-members-of-a-process-group
- https://qiita.com/laikuaut/items/1daa06900ad045d119b4

pgidどうやって取得するのが。。 `ps -o pgid -p $(cat x.pid) | ioknife rest` は無い気がする。

## goで暇な時に

- httpbin
- FileServer
- [x] wire

## go 今日の予定は？

とりあえず既存のWAFのチュートリアルを漁って良さそうなテスト対象を見つける。

### go-chi/chi/_examples ?

https://github.com/go-chi/chi/tree/master/_examples

- [ ] 8.0K	custom-handler -- newTypeでerror対応を付加したServeHTTP()を実装してる
- [ ] 8.0K	custom-method -- これはただGETとか以外のメソッドを追加するだけ
- [ ] 16K	fileserver -- そういえば[fileserver](https://golang.org/pkg/net/http/#example_FileServer)を使った何かをやったことがないな
- [ ] 8.0K	graceful -- http.Server()のShutdownとgoroutine, 
- [ ] 8.0K	hello-world
- [ ] 8.0K	limits
- [ ] 12K	logging
- [ ] 40K	rest
- [ ] 8.0K	router-walk
- [ ] 16K	todos-resource
- [ ] 48K	versions

## docker-compose imageをrecreate

```
$ docker-compose up --force-recreate <image>
```

## arch vpn dns resolution

- https://wiki.archlinux.jp/index.php/OpenVPN#DNS
- https://wiki.archlinux.jp/index.php/Openresolv
- https://wiki.archlinux.jp/index.php/%E3%83%89%E3%83%A1%E3%82%A4%E3%83%B3%E5%90%8D%E5%89%8D%E8%A7%A3%E6%B1%BA

- openresolveを入れると良いっぽい？

```
$ sudo resolveconf -u
```

そもそもopenvpnこの環境では使っていないのか。こういう感じだった。

```console
$ yay -S strongswan xl2tpd networkmanager-l2tp
```

### strongswan

- https://wiki.archlinux.jp/index.php/StrongSwan

/etc/sysctl.d/10-net-forward.conf

```
net.ipv4.ip_forward = 1
net.ipv4.conf.all.accept_redirects = 0
net.ipv4.conf.all.send_redirects = 0
```

/etc/strongswan.d/charon/dhcp.conf

```
dhcp {
 force_server_address = yes
 server = 192.168.0.255
}
```

hmm

### xxx

https://gihyo.jp/admin/serial/01/ubuntu-recipe/0498?page=2

```conosole
$ nmcli device status
$ sudo nmcli connection up <connection name>
```

接続情報はここを見ると分かる。

/etc/NetworkManager/system-connections/xxx

e.g.

```
[connection]
id={VPN NAME}
uuid=xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
type=vpn
autoconnect=false
permissions=user:me:;
timestamp=1565061352

[vpn]
gateway=vpn.example.com
ipsec-enabled=yes
ipsec-esp=3des-sha1!
ipsec-ike=3des-sha1-modp1024!
ipsec-psk={PSK}
password-flags=2
user={USER}
service-type=org.freedesktop.NetworkManager.l2tp

[ipv4]
dns-search=
method=auto

[ipv6]
addr-gen-mode=stable-privacy
dns-search=
method=ignore
```

## hmm

```
$ nslookup <domain> <server>
```


## hostの名前解決に時間が掛かる

timeout オプションを使ってホスト名の検索時間を減らす

ホスト名の解決にとても長い時間がかかっている場合 (pacman やブラウザで)、短い timeout を設定するとよいかもしれません。タイムアウトを設定するには、/etc/resolv.conf に次の行を追加してください:

```
options timeout:1
```

- https://wiki.archlinux.jp/index.php/%E3%83%89%E3%83%A1%E3%82%A4%E3%83%B3%E5%90%8D%E5%89%8D%E8%A7%A3%E6%B1%BA

### あるいは

https://wiki.archlinux.jp/index.php/NetworkManager#.E6.8E.A5.E7.B6.9A.E3.81.AE.E9.AB.98.E9.80.9F.E5.8C.96
