install

```
curl -f https://sh.rustup.rs > ~/Downloads/rust.sh
less ~/Downloads/rust.sh
bash -x ~/Downloads/rust.sh
rustup self update
rustup default stable

cargo install cargo-script
```

hello world
```
cargo script -e 'println!("hello world")'

cargo script 00hello.rs
```
