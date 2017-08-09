## emacs with fcitx

```
LC_CTYPE=ja_JP.UTF-8 XMODIFIERS=@im=fcitx emacs&
```

## rust setup

```
curl -f https://sh.rustup.rs > ~/Downloads/rust.sh
less ~/Downloads/rust.sh
bash -x ~/Downloads/rust.sh
rustup self update
rustup default stable
```

see

- https://wiki.archlinux.org/index.php/Rust

## rust cargo-script

```
cargo install cargo-script
```

### run code (oneline)

like a `python -c 'print("hai")'`

```
$ cargo script -e 'println!("hai")'
```

### run file

```
$ cargo script 00hello.rs
```

## emacs emacsで画像を表示

create-imageを使ってimage data的なものを作りそれをinsert-imageなどで表示する

### 画像の作成

#### file

```lisp
(insert-image (create-image "a.png"))
```
#### data



dataから直接作る事もできる

```lisp
(insert-image (create-image data nil t))
```

別のバッファから直接データを取ってくる場合

```
(string-make-unibyte
  (buffer-substring-no-properties (point-min) (point-max)))
```

サイズも指定できる

```lisp
(insert-image (create-image "a.png" nil nil :max-width 100 :max-height 100))
```

`image-toggle-display-image` のコードを見ると参考になる事がいっぱい

#### memory管理

cacheされた画像を利用するのを止めたければ、image-flushを使う。

### 画像の表示

現在の位置に表示したければ`insert-image`。
中を覗いてみると分かる通りtext-propertyで画像のデータを埋め込んでいる。

```lisp
    (insert string)
    (add-text-properties start (point)
			 `(display ,(if slice
					(list (cons 'slice slice) image)
				      image) rear-nonsticky (display)))))
```

iimage-modeでは以下の様な感じでtoggle

```lisp
  (if arg
      (add-text-properties (match-beginning 0) (match-end 0)
                           `(display ,(create-image file)
                             modification-hooks
                             (iimage-modification-hook)))
    (remove-text-properties (match-beginning 0) (match-end 0)
                            '(display modification-hooks))))))))))
```

詳しい話は[このあたり](https://www.gnu.org/software/emacs/manual/html_node/elisp/Display-Property.html#Display-Property)
sliceについては[このあたりに](https://www.gnu.org/software/emacs/manual/html_node/elisp/Other-Display-Specs.html#Other-Display-Specs) modification-hooksについては[このあたりに](https://www.gnu.org/software/emacs/manual/html_node/elisp/Special-Properties.html#Special-Properties)

### 参考

- iimage-mode
- image-mode
