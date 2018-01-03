# http request中に止める良い方法あったっけ？

- signal.signal SIGINT
- atexit
- KeyboardInterruptを捕捉

# asyncioでのgraceful stopの一例

https://github.com/aaugustin/websockets/blob/master/example/shutdown.py

# bash makefile serverを動かして要らなくなったら止める

```make
00:
	{ python 00hello.py & echo $$! > pid; }
	http :8000/ -b | tee 00.output
	kill `cat pid` && rm pid
```

see [saving PID of spawned process within a Makefile - Stack Overflow](https://stackoverflow.com/questions/23366112/saving-pid-of-spawned-process-within-a-makefile "saving PID of spawned process within a Makefile - Stack Overflow")

# python pandas

- innerjoinでできること
- pandasをimportすると１秒位かかる

## python join

- joinについて(inner joinだけ)
- pandasをいつ使うか
- pandasをどこから使うか

