# docker-compose terminatedになって死んでいる状態

```
$ docker-compose ps
         Name                        Command               State                       Ports                      
-----------------------------------------------------------------------------------------------------------------
my_elastic_1   /docker-entrypoint.sh elas ...   Up      0.0.0.0:9200->9200/tcp, 0.0.0.0:9300->9300/tcp 
my_mongodb_1   /entrypoint.sh mongod            Up      0.0.0.0:27017->27017/tcp                       
```

このmongodb側が死んでいる。terminatedな状態を復帰させてしまっているので再起動しないと多分だめ。

```
$ docker-compose logs | tail -n 3
mongodb_1  | 2016-11-21T02:34:52.619+0000 [initandlisten] connection accepted from 172.17.0.1:60170 #165 (15 connections now open)
mongodb_1  | 2016-11-21T02:35:27.729+0000 [conn165] end connection 172.17.0.1:60170 (14 connections now open)
mongodb_1  | 2016-11-21T02:35:29.427+0000 [signalProcessingThread] got signal 15 (Terminated), will terminate after current cmd ends
```

なるべく中のデータは殺したくないのだけれど。

```
# これがtimeoutになってしまう
$ docker-compose stop mongodb
Stopping aianalystback_mongodb_1 ... 

ERROR: for aianalystback_mongodb_1  UnixHTTPConnectionPool(host='localhost', port=None): Read timed out. (read timeout=70)
ERROR: An HTTP request took too long to complete. Retry with --verbose to obtain debug information.
If you encounter this issue regularly because of slow network conditions, consider setting COMPOSE_HTTP_TIMEOUT to a higher value (current value: 60).

# 消せない
$ docker-compose rm -v --force mongodb
No stopped containers
```

結局docker自身をrestartした。後で真面目な対応把握したい。


# emacs上のansicolorでescapeされた文字列のansicolorを除去する

region選択して `M-|` で `gsed -r "s/\x1B\[([0-9]{1,2}(;[0-9]{1,2})?)?[m|K]//g"`

参考

- [Remove color codes (special characters) with sed | commandlinefu.com](http://www.commandlinefu.com/commands/view/3584/remove-color-codes-special-characters-with-sed)
