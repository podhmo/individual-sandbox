## pagination

- 

## go gocode

```console
$ go get -u -v github.com/mdempsky/gocode
$ go get -u -v github.com/rogpeppe/godef
```

## rust quickrunで実行ファイルが残る

```
    ("rust" . ((:command . "rustc")
               (:exec . ("%c %o -o %e %s" "%e %a"))
               (:compile-only . "%c %o -o %e %s")
               (:remove . ("%e"))
               (:description . "Compile rust and execute")))

;; ここで

(defconst quickrun--template-place-holders
  '("%c" "%o" "%s" "%S" "%a" "%d" "%n" "%N" "%e" "%E")
  "A list of place holders of each language parameter.
Place holders are beginning with '%' and replaced by:
%c: :command parameter
%o: command options
%s: source code name
%S: source code name without extension
%a: program argument
%d: directory name
%n: absolute path of source code without extension
%N: source code path without extension
%e: absolute path of source code with executable extension(.exe, .out, .class)
%E: source code name with executable extension
")
```

つまり

```
;; (:exec . ("%c %o -o %e %s" "%e %a"))
"<compiler> <compiler arguments> -o <executable path> <source file>" "<executable path> <program arguments>"
```

debugしてみる

```lisp
(setq quickrun-debug nil)
```

実行結果

```
Quickrun Execute: rustc -o ./qr_10962m_m.out qr_10962m_m.rs at ./
View mode: type C-h for help, ? for commands, q to quit.
Quickrun Execute: ./qr_10962m_m.out  at ./
View mode: type C-h for help, ? for commands, q to quit.
Quickrun remove (./qr_10962m_m.out ./qr_10962m_m.rs)
```

そもそも何かがおかしい。どこかで謎の実行がある？

```lisp
  (when (boundp 'quickrun--language-alist)
    (setq quickrun--language-alist
          (remove* "rust" quickrun--language-alist :key 'car :test 'equal))
    (add-to-list 'quickrun--language-alist
      '("rust" . ((:command . "rustc-wrap")
                 (:exec . ("%c %o -o %e %s" "%e %a"))
                 (:compile-only . "%c %o -o %e %s")
                 (:remove . ("%e"))
                 (:description . "Compile rust and execute")))))
```

hmmm

```bash
#!/bin/bash
echo $@ >> ~/.cargo/rustc.log
rustc $@
```


## rust by example

- https://doc.rust-lang.org/rust-by-example/

### test

- https://doc.rust-lang.org/book/second-edition/ch11-01-writing-tests.html
- https://rust-lang-ja.github.io/the-rust-programming-language-ja/1.6/book/testing.html

もしかして、cargoを使わないとダメ？

```console
cargo new adder --lib
cd adder
cargo test
   Compiling adder v0.1.0 (file://VENV/individual-sandbox/daily/20180720/example_rust/adder)
    Finished dev [unoptimized + debuginfo] target(s) in 0.17s
     Running target/debug/deps/adder-f131674ce1dd8bed

running 1 test
test tests::it_works ... ok

test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out

   Doc-tests adder

running 0 tests

test result: ok. 0 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
```

## rust emacs

- rustup

```
$ curl https://sh.rustup.rs -sSf | sh
$ vim ~/.bashrc
$ rustup component add rust-src
$ cargo install racer
$ cargo install rustfmt
```

.bashrc or .bash_profile

```bash
## rust
if [ -d $HOME/.cargo ]; then
    source $HOME/.cargo/env
fi
```

### cargo install is failed

```
error: expected ident, found #
  --> $HOME/.cargo/registry/src/github.com-1ecc6299db9ec823/clap-2.32.0/src/app/settings.rs:7:1
   |
7  | / bitflags! {
8  | |     struct Flags: u64 {
9  | |         const SC_NEGATE_REQS       = 1;
10 | |         const SC_REQUIRED          = 1 << 1;
...  |
50 | |     }
51 | | }
   | |_^
   |
   = note: this error originates in a macro outside of the current crate
```


あー

- https://github.com/bitflags/bitflags/issues/138

```console
$ rustc -V
rustc 1.19.0 (0ade33941 2017-07-17)
$ rustup update stable
$ rustc -V
rustc 1.27.1 (5f2b325f6 2018-07-07)
```


### see:

- https://www.rust-lang.org/en-US/install.html
- https://github.com/racer-rust/emacs-racer#installation

