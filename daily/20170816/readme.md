## rust commandline argument

```
use std::env;

env::args().nth(1)
```

## rust ���äƤߤ�

hello world�Τ褦�ʤ�Τ�Ϥ���˽񤯤��ɤ���

```
$ cargo script 00hello.rs
```

���θ�grep�ߤ����ʤ�Τ�����ɤ���

-[����Ū�ʥ��ץꥱ��������񤤤Ƥߤ褦�� Rust�ι�¤���ץ���ߥ󥰡��������Ȥ��Ƥ�Rust�� - ���󥸥˥�Hub�ü��Web���󥸥˥��Υ���ꥢ��ͤ��롪](https://employment.en-japan.com/engineerhub/entry/2017/07/19/110000)

```
cargo new rsgrep --bin
cargo run -- pattern Cargo.toml
cargo build
cargo run -- '^[\[]' Cargo.toml
```

