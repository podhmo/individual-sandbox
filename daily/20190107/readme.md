## python lsp eglotのテストをpythonでsimulateしてみる

lspの理解を調べるために

## emacs現在使われているfontを調べる

```lisp
(frame-parameter nil 'font)
```

## 現在のfont heightを調べる

```lisp
(* (or (nth 2 text-scale-mode-remapping) 1) (face-attribute 'default :height))
```

## emacs fontサイズを大きめにする設定を追加してみる

ただ、IMEの表示は大きくならないのでそのあたりは微妙かもしれない

### 現在のバッファのフォントサイズを相対的に変更

- `C-x +` text-scale-increase
- `C-x -` text-scale-decrease

### 現在のバッファのフォントサイズを絶対的に変更

```lisp
(setq font-height 100)
(face-remap-add-relative 'default :height font-height)
```

### 全てのバッファのフォントサイズを変更

```lisp
(set-face-attribute 'default nil :height font-height)
```

## elisp defun*

```lisp
(defun* f (&optional x &key y) (list x y))
(f);; => (nil nil)
(f 10);; => (10 nil)
(f 10 :y 20);; => (10 20)
```

## elisp stateful function

```lisp
(lexical-let ((i 10))
  (defun f (&optional d) (setq i (+ i (or d 0))) i)
)

(f);; => 10
(f 20);; => 30
(f);; => 30
```

## elisp cycle-list

```lisp
(nth 0 '(x y));; => x
(nth 1 '(x y));; => y
(nth 2 '(x y));; => nil
(nth 3 '(x y));; => nil
```

### font-size

```lisp
(setq my:font-size-candidates '(19 30))

(lexical-let ((i 0))
  (defun* my:font-size (&optional incp &key candidates)
    (when incp
      (setq i (+ i (if (numberp incp) incp 1))))

    (let* ((candidates (or candidates my:font-size-candidates))
           (size (nth i candidates)))
      (cond ((numberp size) size)
            (t
             (setq i 0)
             (nth i candidates))))))

(my:font-size);; => 19
(my:font-size);; => 19
(my:font-size t);; => 30
(my:font-size);; => 30
(my:font-size 1);; => 19
(my:font-size);; => 19
```

この辺closureを返すようにしても良いのかもしれないけれど

## elisp interactive

```lisp
(defun f (&optional x)
(interactive "p")
(insert (format "%s" x)))

;; <call-interactively f>
1
;; C-u <call-interactively f>
4
```

## elisp バッファローカル変数の一覧を表示

```
(buffer-local-variables)
(buffer-local-variables (current-buffer))
```

## emacs 一度きりのkeymapを指定

`set-transient-map` が使える。face-remap.elの関数定義が参考になる。

```lisp
(defun text-scale-adjust (inc)
  (interactive "p")
  (let ((ev last-command-event)
	(echo-keystrokes nil))
    (let* ((base (event-basic-type ev))
           (step
            (pcase base
              ((or ?+ ?=) inc)
              (?- (- inc))
              (?0 0)
              (_ inc))))
      (text-scale-increase step)
      ;; (unless (zerop step)
      (message "Use +,-,0 for further adjustment")
      (set-transient-map
       (let ((map (make-sparse-keymap)))
         (dolist (mods '(() (control)))
           (dolist (key '(?- ?+ ?= ?0)) ;; = is often unshifted +.
             (define-key map (vector (append mods (list key)))
               (lambda () (interactive) (text-scale-adjust (abs inc))))))
         map))))) ;; )
```
