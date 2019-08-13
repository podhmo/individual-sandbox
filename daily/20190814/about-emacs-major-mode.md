#[emacs][memo]個人的なemacsのmajor-modeの作成方法についてのまとめ（リンク集）

なんか忘れてしまったので個人用のまとめ。おそらく他の人に役に立つことは無い。

## emacswikiから

major-modeを定義する方法については概ねemacswikiに説明がある。

- [from scratch](https://www.emacswiki.org/emacs/SampleMode)
- [define-generic-mode](https://www.emacswiki.org/emacs/%E3%82%B8%E3%82%A7%E3%83%8D%E3%83%AA%E3%83%83%E3%82%AF%E3%83%A2%E3%83%BC%E3%83%89)
- [define-derived-mode](https://www.emacswiki.org/emacs/DerivedMode)

define-generic-modeは新しいmajor-modeを一から作るためのhelper。define-derived-modeは既存のmodeを拡張したmajor-modeを作るためのhelper。

## 詳細

### from scratch

https://www.emacswiki.org/emacs/SampleMode

#### other examples

http://www.cs.ise.shibaura-it.ac.jp/wiki/wiki.cgi?page=Emacs%A5%E2%A1%BC%A5%C9%BA%EE%C0%AE%BC%EA%BD%E7

これはスクラッチから定義する別例。

### generic-mode

[define-generic-mode](https://www.emacswiki.org/emacs/%E3%82%B8%E3%82%A7%E3%83%8D%E3%83%AA%E3%83%83%E3%82%AF%E3%83%A2%E3%83%BC%E3%83%89)

```
(define-generic-mode MODE COMMENT-LIST KEYWORD-LIST FONT-LOCK-LIST
AUTO-MODE-LIST FUNCTION-LIST &optional DOCSTRING)
```

#### other examples

https://qiita.com/tm_tn/items/3b40b5ab5e72750ffd7f

これは以下の機能を追加した別例

- eldocとの連携

### derived-mode

[define-derived-mode](https://www.emacswiki.org/emacs/DerivedMode)

```
(define-derived-mode CHILD PARENT NAME &optional DOCSTRING &rest BODY)
```

#### other examples

http://www.wilfred.me.uk/blog/2015/03/19/adding-a-new-language-to-emacs/

これは以下のような機能を追加した別例

- syntax highlight
- indentation
- flycheck (lint)
- completion

### refs

- https://www.emacswiki.org/emacs/SampleMode
- http://www.wilfred.me.uk/blog/2015/03/19/adding-a-new-language-to-emacs/
- http://www.cs.ise.shibaura-it.ac.jp/wiki/wiki.cgi?page=Emacs%A5%E2%A1%BC%A5%C9%BA%EE%C0%AE%BC%EA%BD%E7
- https://qiita.com/tm_tn/items/3b40b5ab5e72750ffd7f
- http://yanqirenshi.hatenablog.com/entry/2018/01/15/Emacs_Lisp_%E3%81%AE%E3%83%A2%E3%83%BC%E3%83%89%E9%96%A2%E9%80%A3%E3%81%AE%E3%82%AA%E3%83%9A%E3%83%AC%E3%83%BC%E3%82%BF

