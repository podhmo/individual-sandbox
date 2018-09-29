## swagger 3.0

https://github.com/podhmo-sandbox/openapi-sandbox

何ができると良いんだろう？とりあえず、手元のツールはopenapi 3.0化しておきたい所。definitionsとかから変更点とか見ていく？それもそれで面倒なのだよなー。何らかのサンプルを集めておきたいような気がする。

めんどくさいことをまとめておきたい気がする。

- そもそもopen api doc書くのがめんどくさい
- yamlのlinterの環境をまともにしたいかも？

簡単なexampleから初めてまともにしていこうかな。

```yaml
# from https://swagger.io/docs/specification/basic-structure/

openapi: 3.0.0
  info:
    title: Sample API
    description: Optional multiline or single-line description in [CommonMark](http://commonmark.org/help/) or HTML.
    version: 0.1.9
  servers:
    - url: http://api.example.com/v1
    description: Optional server description, e.g. Main (production) server
    - url: http://staging-api.example.com
    description: Optional server description, e.g. Internal staging server for testing
  paths:
    /users:
    get:
      summary: Returns a list of users.
      description: Optional extended description in CommonMark or HTML.
      responses:
      '200':  # status code
        description: A JSON array of user names
        content:
        application/json:
          schema: 
          type: array
          items: 
            type: string
```

### 00 basic structure

https://github.com/podhmo-sandbox/openapi-sandbox/tree/master/docs/examples/00basic-structure.yaml

- /swaggerが/openapiになった
- /infoは変わらず
- /hostが/servers/0/urlになった
- /schemesは消えて/servers/0/urlのprefixとして着くようになった
- /basePathも消えて/servers/0/urlのとして着くようになった
- /paths/~1users/get/producesのapplicatin/jsonが消えて/paths/~1users/responses/200/content/application~1json以下にschemaを書くようになった

### 01 parameters, responses

https://github.com/podhmo-sandbox/openapi-sandbox/tree/master/docs/examples/01query-and-body.yaml

- (たしか、queryかなにかの書き方が変わったというような記憶parametersの部分)
- /paths/~1users~1{userId}/get/parameters/0/typeが/paths/~1users~1{userId}/get/parameters/0/schema/typeの方に移動されている
- 200/schemasが変わって200/content/application~1json/schema
- /paths/~1users/post/parameters/0/in/bodyが変わって/paths/~1users/post/requestBody/contentになった

### 02 models

- /definitionsが/components/schemasになった


### 03 authentication

- https://swagger.io/docs/specification/authentication/
- https://swagger.io/docs/specification/2-0/authentication/

yamlの位置をどうにかする何か欲しいな。。

## emacs elispも整形して欲しい

C-x C-sにbindしたい

### elispで式のbegin,endを取るのってどうするのだっけ？

```lisp
        (save-excursion
          (end-of-defun)
          (beginning-of-defun)
          (setq beg (point))
          (setq form (read (current-buffer)))
          (setq end (point)))
```


## emacs そういえばずっとdired経由でのbuffer openに不満が

- https://github.com/syl20bnr/spacemacs/issues/6820
- https://emacs.stackexchange.com/questions/35536/dired-mouse-click-open-folder-in-the-same-window/36330
- https://emacs.stackexchange.com/questions/27712/switch-to-buffer-vs-pop-to-buffer-same-window/27768#27768

shell bufferの表示は以下で大丈夫になるっぽい。

```lisp
(add-to-list 'display-buffer-alist `("\\*shell\\*"  . ,display-buffer-same-window))
```

[display-buffer-alist](https://www.gnu.org/software/emacs/manual/html_node/elisp/Display-Action-Functions.html)の説明を読むと良い。
(追記：いや、[display-buffer](https://www.gnu.org/software/emacs/manual/html_node/elisp/Choosing-Window.html)の説明を読むのが正解かも

dired-find-file

```lisp
(defun dired-find-file ()
  "In Dired, visit the file or directory named on this line."
  (interactive)
  ;; Bind `find-file-run-dired' so that the command works on directories
  ;; too, independent of the user's setting.
  (let ((find-file-run-dired t)
        ;; This binding prevents problems with preserving point in
        ;; windows displaying Dired buffers, because reverting a Dired
        ;; buffer empties it, which changes the places where the
        ;; markers used by switch-to-buffer-preserve-window-point
        ;; point.
        (switch-to-buffer-preserve-window-point
         (if dired-auto-revert-buffer
             nil
           switch-to-buffer-preserve-window-point)))
    (find-file (dired-get-file-for-visit))))
```

(find-file-run-diredはdynamic scope的なoption?)

ちなみにfind-fileは以下の様になっている。

```lisp
(defun find-file (filename &optional wildcards)
  (interactive
   (find-file-read-args "Find file: "
                        (confirm-nonexistent-file-or-buffer)))
  (let ((value (find-file-noselect filename nil nil wildcards)))
    (if (listp value)
	(mapcar 'pop-to-buffer-same-window (nreverse value))
      (pop-to-buffer-same-window value))))
```

find-file-noselectで取り出したbufferにpop-to-buffer-same-windowを呼んでいる？

```lisp
(defun pop-to-buffer-same-window (buffer &optional norecord)
  (pop-to-buffer buffer display-buffer--same-window-action norecord)
```

これ見ると普通に問題なさそうに見えるのだけれど。。？

```lisp
;;; Display + selection commands:
(defun pop-to-buffer (buffer-or-name &optional action norecord)
  (interactive (list (read-buffer "Pop to buffer: " (other-buffer))
		     (if current-prefix-arg t)))
  (let* ((buffer (window-normalize-buffer-to-switch-to buffer-or-name))
         (old-frame (selected-frame))
	 (window (display-buffer buffer action)))
    ;; Don't assume that `display-buffer' has supplied us with a window
    ;; (Bug#24332).
    (if window
        (let ((frame (window-frame window)))
          ;; If we chose another frame, make sure it gets input focus.
          (unless (eq frame old-frame)
            (select-frame-set-input-focus frame norecord))
          ;; Make sure the window is selected (Bug#8615), (Bug#6954)
          (select-window window norecord))
      ;; If `display-buffer' failed to supply a window, just make the
      ;; buffer current.
      (set-buffer buffer))
    ;; Return BUFFER even when we got no window.
    buffer))
```

正体はdisplay-buffer。デフォルトがinhibit-samewindow-tなんだな。

```lisp
(defun display-buffer (buffer-or-name &optional action frame)
  (interactive (list (read-buffer "Display buffer: " (other-buffer))
		     (if current-prefix-arg t)))
  (let ((buffer (if (bufferp buffer-or-name)
		    buffer-or-name
		  (get-buffer buffer-or-name)))
	;; Make sure that when we split windows the old window keeps
	;; point, bug#14829.
	(split-window-keep-point t)
	;; Handle the old form of the first argument.
	(inhibit-same-window (and action (not (listp action)))))
    (unless (listp action) (setq action nil))
    (if display-buffer-function
	;; If `display-buffer-function' is defined, let it do the job.
	(funcall display-buffer-function buffer inhibit-same-window)
      ;; Otherwise, use the defined actions.
      (let* ((user-action
	      (display-buffer-assq-regexp
	       (buffer-name buffer) display-buffer-alist action))
             (special-action (display-buffer--special-action buffer))
	     ;; Extra actions from the arguments to this function:
	     (extra-action
	      (cons nil (append (if inhibit-same-window
				    '((inhibit-same-window . t)))
				(if frame
				    `((reusable-frames . ,frame))))))
	     ;; Construct action function list and action alist.
	     (actions (list display-buffer-overriding-action
			    user-action special-action action extra-action
			    display-buffer-base-action
			    display-buffer-fallback-action))
	     (functions (apply 'append
			       (mapcar (lambda (x)
					 (setq x (car x))
					 (if (functionp x) (list x) x))
				       actions)))
	     (alist (apply 'append (mapcar 'cdr actions)))
	     window)
	(unless (buffer-live-p buffer)
	  (error "Invalid buffer"))
	(while (and functions (not window))
	  (setq window (funcall (car functions) buffer alist)
	  	functions (cdr functions)))
	(and (windowp window) window)))))
```

### 追記

diredに割り当てれば良さそうな気がする

```lisp
;;;###autoload (define-key ctl-x-map "d" 'dired)
;;;###autoload
(defun dired (dirname &optional switches)
  ;; Cannot use (interactive "D") because of wildcards.
  (interactive (dired-read-dir-and-switches ""))
  (pop-to-buffer-same-window (dired-noselect dirname switches)))
```

単純に"a"を使うと言うかdired-find-alternate-fileを使えば良いのかもしれない。

### 追記

そういえば、そもそもfind-fileの問題だったような。

find-fileはpop-to-buffer-same-windowを使っているのだけれど。今使いたいのはswitch-to-buffer?。いや違うな。何かの拡張が悪さをしていそう。pop-win?


## dired jkで移動したい

jk,JK,hとか？
