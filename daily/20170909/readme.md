## python 環境変数込みのパスを利用する

os.path.expandvarsを使えば大丈夫そうだった。


## docker-compose arch

```
pacman -Ss docker-compose
sudo pacman -S docker-compose
sudo systemctl enable docker
```

temporary activate docker

```
sudo systemctl start docker
```

see also

- [Arch Linux — Docker-docs-ja 1.13.RC ドキュメント](http://docs.docker.jp/engine/installation/linux/archlinux.html)
- [Docker - ArchWiki](https://wiki.archlinux.org/index.php/Docker)

memo

dockerのgidにかえるとsudoつける必要はなくなりそう。

```
newgrp docker
```

## python rq

```
pip install rq
rq worker
```

ところで、arqというのもあるっぽい。
後でこちらも触ってみることにしよう。
