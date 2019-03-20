## python asyncio

- SINGINTを受け取る
- add_signal_handler()

※ ProcessExecutorを使う必要なさそう

### やりたいこと

- sub taskを実行する
- sub taskはSIGINTで止められる
- timeoutができるようにする
- Exceptionのときにはraiseする

元のコードがやりたいことがわからない。

### asyncio

add_signal_handler()はsignal.signalと違ってloopに触ってアレコレしても良い
その場ですぐに止めたいという話か

