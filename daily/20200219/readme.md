graphqlでもサイズが大きくなるとだるくない？

## windowsに関する不満

- emacsのファイルIOが遅すぎて無理
- vscodeに移ってみた

  - その場でterminalを開くということが難しい
  - カーソルの移動が遅い気がする
  - Ctrl-p での絞り込み時に更新日で降順にソートされてほしい
  - 適度な思い付きを記録する機能がない
  - フォントサイズが小さい
  - terminal上ではemacsのキーバインディングが効かなくなってしまののがきつい。
  - タブの移動が自分の好みのキーバインディングではないかも？

- 漢字変換の候補選択の部分のカーソルが不慣れ

  - googlen日本語入力はキー設にことえりが選べるらしい。これは素晴らしい。
  - しかしscoopではインストールできなかった。仕方がないので手動でインストールした。
  - 半角全角の押し間違いを良くしてしまう気がする。

## go namedpipe

- https://gist.github.com/matishsiao/fc1601a3a3f37c70d91ab3b1ed8485c4


### windows

- http://bamch0h.hatenablog.com/
- https://github.com/Microsoft/go-winio
- https://github.com/natefinch/npipe


## go jsonrpc

- https://golang.org/pkg/net/rpc/jsonrpc/

### 追記

便利そうなのあるじゃん？(internalだったけど）

- https://github.com/golang/tools/tree/master/internal/jsonrpc2

- https://github.com/golang/tools/blob/0fd2d649e656450a551dd49c43b57425dffd4b53/internal/lsp/cmd/serve.go#L79

hmm

https://github.com/golang/tools/blob/0fd2d649e656450a551dd49c43b57425dffd4b53/internal/jsonrpc2/stream.go

streamというinterfaceを切っているのか。

https://github.com/golang/tools/blob/0fd2d649e656450a551dd49c43b57425dffd4b53/internal/jsonrpc2/stream.go#L22

### net.Pipe

goでnet.Pipe()の代わりのコードを書けないんだろうか？

```
// Conn is a generic stream-oriented network connection.
//
// Multiple goroutines may invoke methods on a Conn simultaneously.
type Conn interface {
	// Read reads data from the connection.
	// Read can be made to time out and return an Error with Timeout() == true
	// after a fixed time limit; see SetDeadline and SetReadDeadline.
	Read(b []byte) (n int, err error)

	// Write writes data to the connection.
	// Write can be made to time out and return an Error with Timeout() == true
	// after a fixed time limit; see SetDeadline and SetWriteDeadline.
	Write(b []byte) (n int, err error)

	// Close closes the connection.
	// Any blocked Read or Write operations will be unblocked and return errors.
	Close() error

	// LocalAddr returns the local network address.
	LocalAddr() Addr

	// RemoteAddr returns the remote network address.
	RemoteAddr() Addr

	// SetDeadline sets the read and write deadlines associated
	// with the connection. It is equivalent to calling both
	// SetReadDeadline and SetWriteDeadline.
	//
	// A deadline is an absolute time after which I/O operations
	// fail with a timeout (see type Error) instead of
	// blocking. The deadline applies to all future and pending
	// I/O, not just the immediately following call to Read or
	// Write. After a deadline has been exceeded, the connection
	// can be refreshed by setting a deadline in the future.
	//
	// An idle timeout can be implemented by repeatedly extending
	// the deadline after successful Read or Write calls.
	//
	// A zero value for t means I/O operations will not time out.
	//
	// Note that if a TCP connection has keep-alive turned on,
	// which is the default unless overridden by Dialer.KeepAlive
	// or ListenConfig.KeepAlive, then a keep-alive failure may
	// also return a timeout error. On Unix systems a keep-alive
	// failure on I/O can be detected using
	// errors.Is(err, syscall.ETIMEDOUT).
	SetDeadline(t time.Time) error

	// SetReadDeadline sets the deadline for future Read calls
	// and any currently-blocked Read call.
	// A zero value for t means Read will not time out.
	SetReadDeadline(t time.Time) error

	// SetWriteDeadline sets the deadline for future Write calls
	// and any currently-blocked Write call.
	// Even if write times out, it may return n > 0, indicating that
	// some of the data was successfully written.
	// A zero value for t means Write will not time out.
	SetWriteDeadline(t time.Time) error
}
```

Listener

```
// A Listener is a generic network listener for stream-oriented protocols.
//
// Multiple goroutines may invoke methods on a Listener simultaneously.
type Listener interface {
	// Accept waits for and returns the next connection to the listener.
	Accept() (Conn, error)

	// Close closes the listener.
	// Any blocked Accept operations will be unblocked and return errors.
	Close() error

	// Addr returns the listener's network address.
	Addr() Addr
}
```

## starlark

- https://github.com/google/starlark-go

## go doc

そういえばgo docってどうなっていたんだっけ？
素直に `go/parser`のParseDir()とかかしてた。

## github api graphql

- https://help.github.com/en/github/authenticating-to-github/creating-a-personal-access-token-for-the-command-line
- https://developer.github.com/v4/
- https://developer.github.com/v4/explorer/

別にpersonal tokenを取ればよいだけっぽい。

### token

full control付きならいけるっぽい

- https://github.com/settings/tokens
- https://gotohayato.com/content/460/

### organization

- https://developer.github.com/v4/query/
- https://developer.github.com/v4/object/organization/

見方がわかってきた。しかしtokenの情報が不足しているようだ。

```
            "message": "Your token has not been granted the required scopes to execute this query. The 'createdAt' fiel
d requires one of the following scopes: ['read:org'], but your token has only been granted the: ['notifications', 'repo
', 'workflow'] scopes. Please modify your token's scopes at: https://github.com/settings/tokens.",
            "type": "INSUFFICIENT_SCOPES
```


### pr

- https://developer.github.com/v4/object/repository/
- https://developer.github.com/v4/enum/pullrequeststate/

## python module

codegen

## go flagの使い方

- subcommand

めんどくさくなって辞めた。でも以前のkingpingのものと同様のものは作れるような気がする。
