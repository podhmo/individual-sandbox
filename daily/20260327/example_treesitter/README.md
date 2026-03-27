# tree-sitter

- https://www.masteringemacs.org/article/lets-write-a-treesitter-major-mode
- https://zenn.dev/link/comments/a11b51216f95b3
- python.el.gz /Applications/Emacs.app/Contents/Resources/lisp/progmodes/python.el.gz

# やりたいこと

- pythonのtree sitterのparserを利用してmini-python-ts-modeを作成したい。
- このmodeはpython-modeを継承しない。
- このmodeはfont-lockの設定だけを使う。

# 実装: mini-python-ts-mode

## ファイル構成

```
mini-python-ts-mode.el   ; メインファイル
sample.py                ; テスト用Pythonコード
```

## 構成

```
(require 'treesit)

defvar mini-python--treesit-keywords    ; キーワードリスト
defvar mini-python--treesit-builtin-types
defvar mini-python--treesit-builtins    ; 組み込み関数リスト
defvar mini-python--treesit-operators   ; 演算子リスト

defvar mini-python--treesit-settings    ; treesit-font-lock-rules
   feature 'comment   → font-lock-comment-face
   feature 'definition → 関数名・クラス名・パラメータ
   feature 'keyword   → キーワード + self
   feature 'string    → font-lock-string-face（シンプル版）
   feature 'builtin   → 組み込み関数
   feature 'constant  → True/False/None
   feature 'number    → 整数・浮動小数点数
   feature 'bracket   → ()[]{}
   feature 'delimiter → ,.:;
   feature 'operator  → 演算子

defvar mini-python--treesit-imenu-settings  ; imenu設定
   "Class"    → class_definition ノード
   "Function" → function_definition ノード（メソッド含む）

define-derived-mode mini-python-ts-mode prog-mode "MiniPy[ts]"
   treesit-simple-imenu-settings を設定
```

## 事前準備

tree-sitter Python grammarが未インストールの場合：

```
M-x treesit-install-language-grammar RET python RET
```

## 使い方

```elisp
;; 1. 読み込み
(load "/path/to/mini-python-ts-mode.el")

;; 2. Pythonファイルを開いて手動で有効化
M-x mini-python-ts-mode

;; または .py ファイルに自動適用する場合
(add-to-list 'auto-mode-alist '("\\.py\\'" . mini-python-ts-mode))
```

その前に `M-x treesit-install-language-grammar` が必要かもしれません。
> ⛔ Warning (treesit): Cannot activate tree-sitter, because language grammar for python is unavailable (not-found): dlopen(...)

またemacsのバージョンと合わない場合にはタグの指定が必要かもです。 (`v0.23.6`がそれです)
> ⛔ Warning (treesit): The installed language grammar for python cannot be located or has problems (version-mismatch): 15

``` emacs-lisp
(unless (treesit-language-available-p 'python)
    (push '(python "https://github.com/tree-sitter/tree-sitter-python" "v0.23.6") treesit-language-source-alist)
    ;; (pop treesit-language-source-alist)
    (treesit-install-language-grammar 'python))
```

その後 `treesit-available-p` で確認してください。

## imenu

`M-x imenu` または `M-x consult-imenu` などで関数・クラスの一覧にジャンプできます。

| カテゴリ | 対象ノード |
|---------|-----------|
| Class | `class_definition` |
| Function | `function_definition`（クラス内メソッドも含む） |

## python-ts-mode との違い

- `python-mode` / `python-base-mode` を継承しない → `prog-mode` から派生
- 文字列のdocstring判定なし（シンプルに `font-lock-string-face` を適用）
- 型ヒント・union型のハイライトなし
- インデント設定なし

## 参考: font-lock feature levels

`treesit-font-lock-level` で有効にするfeatureのレベルを制御できる（デフォルト: 3）。

| レベル | features |
|--------|----------|
| 1 | comment, definition |
| 2 | keyword, string |
| 3 | builtin, constant, number |
| 4 | bracket, delimiter, operator |


