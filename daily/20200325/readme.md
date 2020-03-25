## emacs

- 英語のキーボードでもタブ移動が利くようにしたい

  - ja: `C-;, C-:`
  - en: `C-;, C-'`

### elisp

特定の条件のときだけ値を挿入するみたいなことってできたっけ？

```lisp
`(1 2 3);; => (1 2 3)
`(1 2 ,@'(10) 3);; => (1 2 10 3)
`(1 2 ,@(if (= 0 (cl-random 2)) '(10) nil) 3);; => (1 2 10 3)
```

symbolで分岐するのってどうやるんだっけ？

```lisp
(require 'pcase)

(pcase 'en
  (en 100)
  (ja 200)
  (_ 0))
```

### `'` をキーバインドとして設定するのはどうするんだろう？

- https://www.gnu.org/software/emacs/manual/html_node/elisp/Keyboard-Events.html
- https://www.gnu.org/software/emacs/manual/html_node/elisp/Basic-Char-Syntax.html#Basic-Char-Syntax
- https://www.gnu.org/software/emacs/manual/html_node/elisp/Ctl_002dChar-Syntax.html#Ctl_002dChar-Syntax
- https://uk-ar.hatenadiary.org/entry/20120213/1329138385

```
?\C-x;; => 24

?';; => 39
?:;; => 58
(event-convert-list '(control ?x));; => 24
(event-convert-list '(control hyper ?x));; => 16777240
(event-convert-list '(control ?:))

;; 'を使うとeventとして認識されない？ 英語キーボード
```

なんか上手く動かないのでC-j n, C-j p

### defgroup

使い方とか忘れてしまっているな。。

```lisp
(defgroup my nil
  "My custom settings"
  :prefix "my:")

(defcustom my:keyboard-layout 'ja
  "My keyboard layout setting. This can be `en' or `ja'."
  :type '(choice (const en) (const ja))
  :group 'my
  )
```
