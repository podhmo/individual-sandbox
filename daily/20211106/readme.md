# go

## entをいじってみる

https://entgo.io/docs/getting-started/

```console
$ go mod init m
$ go run entgo.io/ent/cmd/ent init User
# edit ent/schema/user.go
$ go generate ./ent
# edit main.go
$ go run entgo.io/ent/cmd/ent init Car Group
```

## sqlcをいじってみる

- https://conroy.org/introducing-sqlc
- その前にdockerでpostgresqlの環境建てないとだめか
- wsl1上だとdocker立ち上げられないのか

```
docker compose up

The command 'docker' could not be found in this WSL 1 distro.
We recommend to convert this distro to WSL 2 and activate
the WSL integration in Docker Desktop settings.

See https://docs.docker.com/docker-for-windows/wsl/ for details.
```

### 解決したので再開

install

```console
$ go install github.com/kyleconroy/sqlc/cmd/sqlc@latest
```

or 

```console
$ docker pull kjconroy/sqlc
$ docker run --rm -v $(pwd):/src -w /src kjconroy/sqlc generate
```

dbの設定例などはドキュメントに書いてあった。。

https://docs.sqlc.dev/en/latest/tutorials/getting-started-postgresql.html

## 生成などはできた

sqlcでデフォルトで見に行くのが何故かsqlc.jsonだけど生成することはできた。

テーブルを作る必要があるか。

- https://qiita.com/aosho235/items/c657e2fcd15fa0647471

```
psql

> create database pggotest
> \l
> \c pggotest

CREATE TABLE authors (
  id   BIGSERIAL PRIMARY KEY,
  name text      NOT NULL,
  bio  text
);

INSERT INTO authors (name,bio) VALUES ('foo', 'hello');

> \dt
```

## wsl1 -> wsl2

- [Upgrading from WSL1 to WSL2 - DEV Community](https://dev.to/adityakanekar/upgrading-from-wsl1-to-wsl2-1fl9)
- [Docker Desktop WSL 2 backend | Docker Documentation](https://docs.docker.com/desktop/windows/wsl/)

単に以下で良いのか

```console
$ wsl --set-version <distribution> 2
```

ちなみにどういうバージョンでインストールされているかは以下で確認できる

```console
$ wsl -l -v  # wsl --list --verbose
```

もうちょっと作業が必要。

```
docker compose up

The command 'docker' could not be found in this WSL 2 distro.
We recommend to activate the WSL integration in Docker Desktop settings.

See https://docs.docker.com/docker-for-windows/wsl/ for details.
```

## posgresqlの環境を作る

- [PostgreSQLを起動するためのdocker-compose.yml - pione30’s blog](https://pione30.hatenablog.com/entry/2020/07/28/235928)
- [docker-composeで作成されるものの名前を明示的に指定する方法 - Qiita](https://qiita.com/satodoc/items/188a387f7439e4ec394f)

テキトーにdocker-compose.ymlを書く
テキトーにクライアントをインストールして確認。 (psql)

```console
$ sudo apt install postgresql-client
```

### 立ち上げたimage

どうやってunix domain socketにアクセスすれば良いんだろ？

```
listening on Unix socket "/var/run/postgresql/.s.PGSQL.5432"
```

### minimum な環境で試す

https://qiita.com/Teramonte4/items/d2ef0fad11a55c125d9f

```console
$ docker run -d --rm --name db -p 5555:5432 -e POSTGRES_HOST_AUTH_METHOD=trust postgres

# docker ps
$ docker exec -it db bash
  $ psql -U postgres
```

POSTGRES_HOST_AUTH_METHODがわかっていないのか。

### 改めてdocker compose

docker-compose.ymlに

- hostnameをつけている記事がある
- networkをつけている記事がある -- bridge


これらはdocker composeで自動的に付与される（うざいときもある）
どうやらわかっていないのはdockerのネットワーク周り

### docker network

- https://gist.github.com/miliya612/ac13a1f0e09de78c1ec40516af6e8f8b
- [Dockerのネットワークを理解するために覚えたことまとめ - Carpe Diem](https://christina04.hatenablog.com/entry/2016/07/22/193000)

外部 -> 内部 (host -> guest)は通常はport forwardingで解決するがこれがpsqlだとうまく行っていない感。

```console
$ docker run -p 80:80 --name nginx nginx
$ sudo iptables -t nat -L -n
```

いや、単純に `-h localhost` を指定する必要があるだけか。。

内部 -> 内部

```
$ docker network create dns
$ docker run --net dns --name web1 -it ubuntu:trusty
# ping web1
```

ネットワークに所属しているnodeはどうやって調べるんだろう？
docker inspectでcontainerを指定？。

ネットワークは追加したほうが楽そう。

```console
$ docker network ls
NETWORK ID     NAME                 DRIVER    SCOPE
51f71e09c1c5   bridge               bridge    local
a576cd09a746   example_go_db-net    bridge    local
dc7b3e10f9e5   example_go_default   bridge    local
e3b57077001f   host                 host      local
58a5df9ab086   none                 null      local
```

## ubuntu

- https://snowtree-injune.com/2020/10/09/ubuntu-upgrade-dj007/

```console
$ sudo do-release-upgrade -c
```