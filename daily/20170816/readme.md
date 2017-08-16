## rust commandline argument

```
use std::env;

env::args().nth(1)
```

## rust 触ってみる

hello worldのようなものをはじめに書くと良い？

```
$ cargo script 00hello.rs
```

その後grepみたいなものを作ると良い？

-[実践的なアプリケーションを書いてみよう！ Rustの構造化プログラミング【第二言語としてのRust】 - エンジニアHub｜若手Webエンジニアのキャリアを考える！](https://employment.en-japan.com/engineerhub/entry/2017/07/19/110000)

```
cargo new rsgrep --bin
cargo run -- pattern Cargo.toml
cargo build
cargo run -- '^[\[]' Cargo.toml
```

