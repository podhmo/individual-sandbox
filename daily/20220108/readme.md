## PWAをlocalhostで

はじめは頑張ってlocalhostでself signed certificationをやっていたがchromeのオプションで十分かもしれない。

```console
$ python -m http.server 5555
```

```console
$ chrome.exe --user-data-dir=/tmp/foo --allow-insecure-localhost --unsafely-treat-insecure-origin-as-secure=http://localhost:5555 http://localhost:5555/pwa/hello
```

## pythonでhttps

最初は真面目にhttpsのサーバーを立てようとした。

- threadingじゃないと厳しいかも
- あとなんだか反応がないことがある？
- まだchromeは足りないっぽい？ DomExceptionがでる service workerの登録で

```
localhost/:1 Uncaught (in promise) DOMException: Failed to register a ServiceWorker for scope ('https://localhost:4444/pwa/hello/') with script ('https://localhost:4444/pwa/hello/sw.js'): An SSL certificate error occurred when fetching the script.
```

そして↑のhttpでやる方向に落ち着く。

## go parse, not validate をgoでやってみる

- なかなか厳しい
- nil panicを許容するとそれっぽくなる。何も利用せずに戻り値として使われると死ぬ
- 最終的にsum typeが欲しくなる
