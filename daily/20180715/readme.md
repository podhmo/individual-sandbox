## tcpdump コード変えずに調べられるのでは？

```console
# sudo pacman -Sy tcpdump
# localhostのpacket見る
$ sudo tcpdump -i lo -n port 18125 -v
```

## statsd

```
$ git clone git@github.com:etsy/statsd.git
$ cd statsd
$ npm install
$ node stats.json config_udp.json
```

config_udp.json

```
{
  "debug": true,
  "flushInterval": 10000,
  "graphiteHost": "127.0.0.1",
  "graphitePort": 12003,
  "log": {
    "level": "LOG_DEBUG"
  },
  "port": 18125,
  "mgmt_port": 18126,
  "servers": [
    {
      "address": "0.0.0.0",
      "port": 18125,
      "server": "./servers/udp"
    }
  ]
}
```

debug logの表示の仕方

- "config.json#/log/level" に `"LOG_DEBUG"` を
- "config.json#/debug" に `true` を

動作するサーバー

- mgmt_server
- server

mgmt_serverにはtcpで投げる（デフォルトは`127.0.0.1:8126` configで変えられる)

```
echo help | nc -w 1 127.0.0.1 8126
```

利用できるコマンドは以下(helpをsendすればわかる)

- help
- config
- health
- stats
- counters
- timers
- gauges
- delcounters
- deltimers
- delgauges
- quit


## memory 使用量

https://unix.stackexchange.com/questions/554/how-to-monitor-cpu-memory-usage-of-a-single-process


## memusage

memusageたのしい


## statsd

```console
$ git clone git@github.com:etsy/statsd.git
```

## netcat

- https://msmania.wordpress.com/tag/ncat/

## docker-compose

- https://docs.docker.com/compose/compose-file/#short-syntax
- http://docs.docker.jp/compose/gettingstarted.html

## ps -H

https://blog.adachin.me/archives/8376


## grafanaをlocalで動かす

```console
# $ docker run -d --name=grafana -p 3000:3000 grafana/grafana
$ docker run --name=grafana -p 3000:3000 grafana/grafana

$ docker volume create grafana-storage
$ docker run -p 3000:3000 --name=grafana -v grafana-storage:/var/lib/grafana grafana/grafana
```

admin/admin

- https://github.com/grafana/grafana-docker
- http://docs.grafana.org/installation/docker/#grafana-container-with-persistent-storage-recommended

docker-compose作ったほうが楽じゃん？

- https://www.linode.com/docs/uptime/monitoring/install-graphite-and-grafana/

graphiteもいる root/root. (http://localhost:12080/admin/)

### 追記

- [docker-compose.yml](./docker-compose.yml)

```console
# sudo pacman -Sy openbsd-netcat
$ while true; do echo -n "stats.example:$((RANDOM % 100))|c" | nc -w 1 -u 127.0.0.1 18125; done
```

- http://localhost:12080/render?from=-10mins&until=now&target=stats.example
- https://github.com/etsy/statsd/blob/master/docs/metric_types.md

う〜ん。statsd経由で送るのむずい？

```
$ docker-compose run --rm graphite bash
# cat /opt/statsd/config_udp.json
{
  "graphiteHost": "127.0.0.1",
  "graphitePort": 2003,
  "port": 8125,
  "flushInterval": 10000,
  "servers": [
    { server: "./servers/udp", address: "0.0.0.0", port: 8125 }
  ]
}
```

```
(echo -n "example:10|c" | nc -u 127.0.0.1 18125) && echo ok
```

hmm。まじめにdockerfile見るか。

https://github.com/graphite-project/docker-graphite-statsd/blob/master/Dockerfile

普通にstats.jsをよんでいるだけだな

/etc/service/stats/run

```
#!/bin/bash

exec /usr/bin/nodejs /opt/statsd/stats.js /opt/statsd/config_$STATSD_INTERFACE.js >> /var/log/statsd.log 2>&1
```

手に入るlogはこれだけ

```
15 Jul 00:44:26 - [42] reading config file: /opt/statsd/config_udp.js
15 Jul 00:44:26 - server is up INFO
```

わかんない

### 追記 graphiteに直接書く

```console
# metric_path value timestamp\n
echo "test.bash.stats 42 `date +%s`" | nc -w 1 localhost 12003

while true; do echo "test.random.stats $((RANDOM % 100)) `date +%s`" | nc -w 1 127.0.0.1 12003; done
```

- https://graphite.readthedocs.io/en/latest/feeding-carbon.html#getting-your-data-into-graphite

なんか表示されたようなされていないような

```
while true; do echo "foo.bar $((RANDOM % 100)) `date +%s`" | nc -w 1 127.0.0.1 12003 && echo ok; done
```

改行が入っていなかった。

##  追記

diamondとか使えばよいのでは？

https://github.com/python-diamond/diamond

python2.xだけだった。


## docker 色々削除

```console
# image
$ docker images
$ docket rmi <>

# container
$ docker ps -a
$ docker container prune
```
