## emacs fontset

- fonsetという大きな概念がある
- それらの内個々のcharacter-setに対してfontが割り当てられる

というようなイメージ。

```lisp
;; nameがtの場合にはdefault, nameがnilの場合は現在のframeのfontset
;; (set-fontset-font NAME TARGET FONT-SPEC &optional FRAME ADD)
(set-fontset-font t 'japanese-jisx0208 "TakaoPGothic")
```

現在のfontsetでのfontの設定を調べるのは以下

```lisp
(fontset-font t ?j)
(fontset-font t ?あ)
```

character setの取り出し方は以下

```
(list-character-sets t)
```

```
(let ((xs (loop for c in (charset-list)
                when (string-match-p "jis" (symbol-name c))
                collect c
                )))
  (insert "\n")
  (dolist (x xs)
    (insert (format ";; - %S\n" x))))
;; - katakana-sjis
;; - japanese-jisx0213\.2004-1
;; - japanese-jisx0213-a
;; - japanese-jisx0213-2
;; - japanese-jisx0213-1
;; - japanese-jisx0212
;; - japanese-jisx0208-1978
;; - japanese-jisx0208
;; - katakana-jisx0201
;; - latin-jisx0201
;; - jisx0201
```

unicodeで指定しちゃうのが良いのでは？という話。

- http://extra-vision.blogspot.com/2016/07/emacs.html

記号だけおかしな表示になる場合

- http://misohena.jp/blog/2017-09-26-symbol-font-settings-for-emacs25.html

## emacs でC-x 5 2が動かない(make-frame-command)

おそらく設定されているfontsetが登録されていない。

```
;; 現在利用できるのfontsetを確認する
(call-interactively 'list-fontsets)

;; 現在利用しているfontsetを確認する
(frame-parameter nil 'font)
;; => "-PfEd-DejaVu Sans Mono-normal-normal-normal-*-19-*-*-*-m-0-iso10646-1"

;; 現在のfontsetについて詳しく見る
(describe-fontset nil)
```

設定の仕方だけを知りたいなら

- https://knowledge.sakura.ad.jp/8494/ これが一番参考になるかも？
- http://extra-vision.blogspot.com/2016/07/emacs.html

### fontsetを登録するには？

以下の関数で返ってくれば良い。 (shell上でのfc-listの結果より少ないように見える)

```
(fontset-list)

;create-fontset-from-fontset-spec
	FONTSET-NAME[,SCRIPT-NAME0:FONT-NAME0,SCRIPT-NAME1:FONT-NAME1] ...
;frame-listのfontに入れてあげれば良い
```

```c
DEFUN ("fontset-list", Ffontset_list, Sfontset_list, 0, 0, 0,
       doc: /* Return a list of all defined fontset names.  */)
  (void)
{
  Lisp_Object fontset, list;
  int i;

  list = Qnil;
  for (i = 0; i < ASIZE (Vfontset_table); i++)
    {
      fontset = FONTSET_FROM_ID (i);
      if (!NILP (fontset)
	  && BASE_FONTSET_P (fontset))
	list = Fcons (FONTSET_NAME (fontset), list);
    }

  return list;
}
```

## emacs systemで分岐

```
system-type;; => gnu/linux
;; OS-X なら darwin
```

## emacs font設定

```
(describe-face 'default)

(set-face-attribute 'default nil) ...
(set-face-attribute 'default
  :family "DejaVu Sans Mono"
  :foundry "PfEd"
  :width 'normal
  :slant 'normal
) 

(frame-parameter nil 'font);; => "-PfEd-DejaVu Sans Mono-normal-normal-normal-*-19-*-*-*-m-0-iso10646-1"
(font-spec :family "DejaVu Sans Mono")

(fontset-plain-name (frame-parameter nil 'font))
```

describe-font

```
name (opened by): -PfEd-DejaVu Sans Mono-normal-normal-normal-*-19-*-*-*-m-0-iso10646-1
       full name: DejaVu Sans Mono:pixelsize=19:foundry=PfEd:weight=normal:slant=normal:width=normal:spacing=100:scalable=true
       file name: /usr/share/fonts/TTF/DejaVuSansMono.ttf
            size: 19
          height: 23
 baseline-offset:  0
relative-compose:  0
  default-ascent:  0
          ascent: 18
         descent:  5
   average-width: 11
     space-width: 11
       max-width: 11
```

```
Defined in ‘faces.el’.

           Family: DejaVu Sans Mono
          Foundry: PfEd
            Width: normal
           Height: 143
           Weight: normal
            Slant: normal
       Foreground: white
DistantForeground: unspecified
       Background: dark slate gray
        Underline: nil
         Overline: nil
   Strike-through: nil
              Box: nil
          Inverse: nil
          Stipple: nil
             Font: #<font-object -PfEd-DejaVu Sans Mono-normal-normal-normal-*-19-*-*-*-m-0-iso10646-1>
          Fontset: -PfEd-DejaVu Sans Mono-normal-normal-normal-*-19-*-*-*-m-0-fontset-auto1
          Inherit: nil
```

## emacs C-x 5 2 が動かない

```lisp
(make-frame frame-initial-frame-alist)
```

```
x-create-frame-with-faces: The default fontset can’t be used for a frame font
```

- https://qiita.com/VienosNotes/items/0194772d903873a8d7be
- http://asukiaaa.blogspot.com/2017/11/ubuntuemacs.html

hmm `list-fontsets`

```
Fontset: -PfEd-DejaVu Sans Mono-normal-normal-normal-*-19-*-*-*-m-0-fontset-auto1
Fontset: -*-*-*-*-*-*-*-*-*-*-*-*-fontset-default
Fontset: -*-fixed-medium-r-normal-*-16-*-*-*-*-*-fontset-standard
Fontset: -PfEd-DejaVu Sans Mono-normal-normal-normal-*-15-*-*-*-m-0-fontset-startup
```

```
(print (string-join (font-family-list)))
```

;; (dolist (x (x-list-fonts "*")) (print x))

### 追記まじめに調べる

(find-file "fontset.el")

```console
$ emacs -q -l fontset.el
# C-x 5 2
```

## emacs arch source codeどこにあるんだろう？

downloadのみをする

```
pacman -Sw emacs
```

リストを見る

```
pacman -Ql emacs
```

yayでも良い。

## pacman 検索

local

```
pacman -Qs <word>
```

remote (via package file name)

```
pacman -Fs <word>
```

情報(info)

```
# remote
pacman -Si <package name>

# local
pacman -Qi <package name>
```

- [pacman - ArchWiki](https://wiki.archlinux.jp/index.php/Pacman "pacman - ArchWiki")
