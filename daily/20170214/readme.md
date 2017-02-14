# docker 動いているcontainerに入る

```
$ docker images
# container id調べる
$ docker ps
$ docker exec -it <container id> bash
```

# python stringをboolに

```
from distutils.util import strtobool


bool(strtobool("t"))
```

# emacs ansicolorをhighlightさせたい

```
(require 'ansi-color)

(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))
```

# elastic search とりあえず一覧全部返す

```
http get "http://localhost:9200/<index>/_search/?pretty=true&q*:*"
```

memoどこかでqueryの情報をまとめておきたい。

# elastic search mappingの情報見る

```
http get http://localhost:9200/<index>
```

# elastic search indexの状態を一覧で見る

```
http get http://localhost:9200/_cat/indices
```
