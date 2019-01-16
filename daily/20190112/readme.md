## emacs package-list-auto-remove

## python jsonrpc

なんでwsgi的なinterfaceになっていないんだろうと思ったけれど。そもそも
language serverはJSONRPCの解釈ってあんまり気にしなくても良いっぽい。ど
ちらかと言うとstdin/stdoutとの連携をどうするかという部分の方が重要そう
な感じ。

あとはclient側がしっかりとasyncでやれるというのが良いものっぽい。
asyncな形でのdispatcherを考えてみたいな。

## python python-jsonrpc-servfer

jsonrpc2.0サーバーを用意しないと。

```
pip install python-jsonrpc-server
```

- https://pypi.org/project/python-jsonrpc-server/

どうせなのでpylsに使われている依存を見るか。意外と実装がだるいな。JSONRPCのrequestってどんな形式だっけ？
(この辺reqtraceで見れないかな)

まじめに中を覗くか。

### structure

```console
$ pyinspect list
jsonrpc
jsonrpc.exceptions
jsonrpc.dispatchers
jsonrpc.endpoint
jsonrpc.streams
```

exceptionsってなんだろう？けっこうマジで定義されていた。とりあえず `JsonRpcException` のエラーが使われているっぽい。

```python
class JsonRpcException(Exception):

    def __init__(self, message=None, code=None, data=None):
        super(JsonRpcException, self).__init__(message)
        self.message = message or getattr(self.__class__, 'MESSAGE')
        self.code = code or getattr(self.__class__, 'CODE')
        self.data = data
```

それぞれの意味を把握していきたい。streams,endpoint,dispatchers

### streams

それぞれReaderとWriterが定義されている。どういうinterfaceになっているんだろう？

```console
$ pyinspect resolve jsonrpc.streams
jsonrpc.streams.JsonRpcStreamReader <- builtins.object
    [method, OVERRIDE] __init__(self, rfile)
    [method] close(self)
    [method] listen(self, message_consumer)
        [method] _read_message(self)
            [static method] _content_length(line)

----------------------------------------
jsonrpc.streams.JsonRpcStreamWriter <- builtins.object
    [method, OVERRIDE] __init__(self, wfile, **json_dumps_args)
    [method] close(self)
    [method] write(self, message)

----------------------------------------
```

こういう感じ？

```python
class Closer(Protocol):
    def close(self):
        ...

class Reader(Closer):  # ReaderといいつつListener?
    def listen(self, message_consumer: Callable[Dict]) -> None:
        ...

class Writer(Closer):
    def write(self: message: Dict) -> None:
        ...
```

### dispatcher

dispatcherはともかくendpointはわからない。

1. `m_foo` 的なメソッドを呼ぶ
2. camelcase -> snakecase的な変換が入る

毎回closure生成するのか。。

```python
    """JSON RPC dispatcher that calls methods on itself.

    Method names are computed by converting camel case to snake case, slashes with double underscores, and removing
    dollar signs.
    """

    def __getitem__(self, item):
        method_name = 'm_{}'.format(_method_to_string(item))
        if hasattr(self, method_name):
            method = getattr(self, method_name)

            @functools.wraps(method)
            def handler(params):
                return method(**(params or {}))

            return handler
        raise KeyError()
```

### endpoint

```console
$ pyinspect inspect jsonrpc.endpoint
jsonrpc.endpoint.Endpoint <- builtins.object
    [method, OVERRIDE] __init__(self, dispatcher, consumer, id_generator=<function Endpoint.<lambda> at 0x7f8bca0bd268>, max_workers=5)
    [method] consume(self, message)
        [method] _handle_notification(self, method, params)
            [method] _handle_cancel_notification(self, msg_id)
            [static method] _notification_callback(method, params)
        [method] _handle_response(self, msg_id, result=None, error=None)
        [method] _handle_request(self, msg_id, method, params)
            [method] _request_callback(self, request_id)
    [method] request(self, method, params=None)
        [method] _cancel_callback(self, request_id)
            [method] notify(self, method, params=None)
    [method] shutdown(self)
```

基本的にはconsumeとnotifyに諸々が担われているっぽい。
`request()`の使いみちがわからない。

## emacs pcase

- pcaseの使いかた

- https://www.emacswiki.org/emacs/PatternMatching

## emacs jsonrpc

外部パッケージ

```
M-x package-install jsonrpc
```

jsonrpc2.0らしい。

> This library implements the JSONRPC 2.0 specification as described  in http://www.jsonrpc.org/. 

```console
$ cat jsonrpc.el | grep -P '^\((cl\-|)def' | grep -vP '\-{2}'
(defclass jsonrpc-connection ()
(cl-defmethod initialize-instance ((conn jsonrpc-process-connection) slots)
(cl-defmethod jsonrpc-connection-send ((connection jsonrpc-process-connection)
(cl-defmethod jsonrpc-running-p ((conn jsonrpc-process-connection))
(cl-defmethod jsonrpc-shutdown ((conn jsonrpc-process-connection)

(defclass jsonrpc-process-connection (jsonrpc-connection)

(cl-defgeneric jsonrpc-connection-send (conn &key id method params result error)
(cl-defgeneric jsonrpc-shutdown (conn)
(cl-defgeneric jsonrpc-running-p (conn)
(cl-defgeneric jsonrpc-connection-ready-p (connection what)
(cl-defmacro jsonrpc-lambda (cl-lambda-list &body body)

(defun jsonrpc-events-buffer (connection)
(defun jsonrpc-forget-pending-continuations (connection)
(defun jsonrpc-connection-receive (connection message)
(defun jsonrpc-error (&rest args)

(cl-defun jsonrpc-async-request (connection
(cl-defun jsonrpc-request (connection
(cl-defun jsonrpc-notify (connection method params)

(defconst jrpc-default-request-timeout 10

(defun jsonrpc-process-type (conn)
(defun jsonrpc-stderr-buffer (conn)

(define-error 'jsonrpc-error "jsonrpc-error")
```

### 重要そうなものだけ

重要そうなものだけを抜き出したい。

- jsonrpc-connection (class)
- jsonrpc-request
- jsonrpc-async-request
- jsonrpc-notify
- (jsonrpc-process-type)

### debugを有効にする方法

hmm?
```
(jsonrpc-events-buffer connection)
```

### 詳細

知らないdefあるっけ？

- defclass
- cl-defun (defun*)
- cl-defgeneric
- cl-defmacro (defun*)
- defconst
- define-error

なるほどdefun*などはcl-defunを使えということになっているのか。

### defclass

eieio.elで定義されるもの。

```lisp
(defclass NAME SUPERCLASSES SLOTS &rest OPTIONS-AND-DOC)

Define NAME as a new class derived from SUPERCLASS with SLOTS.
OPTIONS-AND-DOC is used as the class’ options and base documentation.
SUPERCLASSES is a list of superclasses to inherit from, with SLOTS
being the slots residing in that class definition.  Supported tags are:

  :initform   - Initializing form.
  :initarg    - Tag used during initialization.
  :accessor   - Tag used to create a function to access this slot.
  :allocation - Specify where the value is stored.
                Defaults to ‘:instance’, but could also be ‘:class’.
  :writer     - A function symbol which will ‘write’ an object’s slot.
  :reader     - A function symbol which will ‘read’ an object.
  :type       - The type of data allowed in this slot (see ‘typep’).
  :documentation
              - A string documenting use of this slot.

The following are extensions on CLOS:
  :custom     - When customizing an object, the custom :type.  Public only.
  :label      - A text string label used for a slot when customizing.
  :group      - Name of a customization group this slot belongs in.
  :printer    - A function to call to print the value of a slot.
                See ‘eieio-override-prin1’ as an example.
```

### cl-defgeneric

### defconst

ただの定数定義。これはeieioなどの定義ではなさそう。復習。初期値で常に初期化される。

```lisp
;; (defconst SYMBOL INITVALUE [DOCSTRING])
```

### define-error

これはcondition-caseで扱うsignalの定義

```lisp
(define-error 'jsonrpc-error "jsonrpc-error")

;; 以下の様な形で使う
;; (signal 'jsonrpc-error <data>)
```

## emacs 分かっていなそうなパッケージ

- cl-lib
- eieio

### cl-lib

require clを取り除く系の話

### eieio

defclassだけ確認すれば良い？

- https://www.gnu.org/software/emacs/manual/html_node/eieio/

## emacs projectという概念

```
(project-current)
```

## emacs eglot コードの補完を良い感じの単位で切り分ける

```lisp
(ert-deftest basic-completions ()
  "Test basic autocompletion in a python LSP"
  (skip-unless (executable-find "pyls"))
  (eglot--with-fixture
      '(("project" . (("something.py" . "import sys\nsys.exi"))))
    (with-current-buffer
        (eglot--find-file-noselect "project/something.py")
      (should (eglot--tests-connect))
      (goto-char (point-max))
      (completion-at-point)
      (should (looking-back "sys.exit")))))
```

んー。eglotなしで直接サーバにrequestしてみるか。`eglot-connect`を読んでいけば良いはず。

```lisp
;; managed-major-modeというのは文字列？
;; projectはdirectory?
;; classは?
;; contactはたぶん接続情報

(defun eglot--connect (managed-major-mode project class contact)
  "Connect to MANAGED-MAJOR-MODE, PROJECT, CLASS and CONTACT.
This docstring appeases checkdoc, that's all."
...
)
```

ちょっといきなり内部にたどり着きすぎたかも。もう少し外側の部分を。

```lisp
;; 変更されるhookは以下
;; - xref-find-definitions
;; - flymake-mode
;; - eldoc-mode
;; - completion-at-point
;;
;; - classとcontactはeglot-server-programsから補完される
;; - projectはproject-curentから補完される
;; - (classはeglot-lsp-serverのsubclass)
;; - (contactはplistかふつうのlist)
(defun eglot (managed-major-mode project class contact &optional interactive)
  (interactive (append (eglot--guess-contact t) '(t)))
  (let* ((current-server (eglot--current-server))
         (live-p (and current-server (jsonrpc-running-p current-server))))
    (if (and live-p
             interactive
             (y-or-n-p "[eglot] Live process found, reconnect instead? "))
        (eglot-reconnect current-server interactive)
      (when live-p (ignore-errors (eglot-shutdown current-server)))
      (eglot--connect managed-major-mode project class contact))))
```

guess部分を見ると良いかも。

```lisp
;; managed-modeは(eglot-all-major-modes) から見てく
;; projectは(project-current)
;; その他はeglot-server-programsあたりから探す

;; 最終的には以下の様なものが見つかる
(list managed-mode project class contact)
```

eglot-server-programsは

```lisp
;; defaultは以下の様な形(python部分)

(defvar eglot-server-programs '(
  (python-mode . ("pyls"))
))

;; 正確には (<mode> . <contact>) という形式
;; ここで <contact> は以下のどれか
;; 1. (<program> [args ...])
;; 2. (<host> <port> [tcp-args ...]
;; 3. (<program> [args ...] :autoport [more-args ...])
;; 4. (<class-name> . <init-args>)
;; 5. <function(symbol)> ;; (lambda (contact) ...)
;;   (eglotから呼ばれるとnon nil。eglot-ensureから呼ばれるとnil)
```

改めてeglot--guess-contactを見ると以下の様な値が返ってくることが分かる。

```lisp
;; (list managed-mode project class contact)
`(python-mode (vc . "./xxx.py") 'eglot-lsp-server ("pyls"))
```

ようやく、冒頭の関数の定義を見ることができる。

1. contactからprocessの指定を生成する。以下のようなもの

```lisp
`(:process ,(lambda () (make-process
   :name "EGLOT (<dirname>/python-mode)"
   :command ("pyls")
   :connection-type 'pipe
   :coding 'utf-8-emacs-unix
   :noquery t ;; この意味がわからない
   :stderr (get-buffer-create (format "*EGLOT (<dirname>/python-mode) stderr")
```

2. spread とは?
3. serverがmake-instanceで作られる(cacheが使われたりするんだろうか？), spreadを使っている部分の定義が分らず

```lisp
(make-instance 'eglot-lsp-server
  :name EGLOT (<dirname>/python-mode
  :events-buffer-scrollback-size 2000000
  :notification-dispatcher (funcall spread #'eglot-handle-notification)
  :requests-dispatcher (funcall spread #'eglot-handle-request)
  :on-shutdown #'eglot--on-shutdown))
```

ここでspreadは以下の様な定義。単にparamsをbindして呼ぶだけ。(こういうのをspreadって名付けられるの羨ましい)

```lisp
(spread (lambda (fn) (lambda (server method params)
  (apply fn server method (append params nil)))))
```

hmm. `(tag (make-symbol "connected-catch-tag"))` が気になる

そろそろまじめにinstance部分を把握したい

### make-instance

```lisp
(defclass eglot-lsp-server (jsonrpc-process-connection)
  ((project-nickname
    :documentation "Short nickname for the associated project."
    :accessor eglot--project-nickname)
   (major-mode
    :documentation "Major mode symbol."
    :accessor eglot--major-mode)
   (capabilities
    :documentation "JSON object containing server capabilities."
    :accessor eglot--capabilities)
   (shutdown-requested
    :documentation "Flag set when server is shutting down."
    :accessor eglot--shutdown-requested)
   (project
    :documentation "Project associated with server."
    :accessor eglot--project)
   (spinner
    :documentation "List (ID DOING-WHAT DONE-P) representing server progress."
    :initform `(nil nil t) :accessor eglot--spinner)
   (inhibit-autoreconnect
    :initform t
    :documentation "Generalized boolean inhibiting auto-reconnection if true."
    :accessor eglot--inhibit-autoreconnect)
   (file-watches
    :documentation "Map ID to list of WATCHES for `didChangeWatchedFiles'."
    :initform (make-hash-table :test #'equal) :accessor eglot--file-watches)
   (managed-buffers
    :documentation "List of buffers managed by server."
    :accessor eglot--managed-buffers)
   (saved-initargs
    :documentation "Saved initargs for reconnection purposes."
    :accessor eglot--saved-initargs)
   (inferior-process
    :documentation "Server subprocess started automatically."
    :accessor eglot--inferior-process))
  :documentation
  "Represents a server. Wraps a process for LSP communication.")
```

### process management

どうも独自のhash tableにprocessは格納しているみたい。
そしてbuffer local variablesにcacheしている。

```lisp
;;; Process management
(defvar eglot--servers-by-project (make-hash-table :test #'equal)
  "Keys are projects.  Values are lists of processes.")

(defun eglot--current-server ()
  "Find the current logical EGLOT server."
  (or
   eglot--cached-current-server
   (let* ((probe (or (project-current)
                     `(transient . ,default-directory))))
     (cl-find major-mode (gethash probe eglot--servers-by-project)
              :key #'eglot--major-mode))))

(defvar-local eglot--cached-current-server nil
  "A cached reference to the current EGLOT server.
Reset in `eglot--managed-mode-onoff'.")
```

いつ作られるかと言うとはじめに通信したタイミング

```
(defun eglot--current-server-or-lose ()
  "Return current logical EGLOT server connection or error."
  (or (eglot--current-server)
      (jsonrpc-error "No current JSON-RPC connection")))
```

### 通信

通信は以下の様な形

```lisp
(jsonrpc-async-request
  server
  :initialize (list :processId (emacs-pid)
                    :rootPath (expand-file-name default-directory)
                    :rootUri (eglot--path-to-uri default-directory)
                    :initializationOptions (eglot-initialization-options server)
                    :capabilities (eglot-client-capabilities server)
              )
  :success-fn (...)
  :timeout 30
  :error-fn (...)
  :timeout-fn (...))
|> retval

pcase error, nil, default
```

いつjsonrpcを閉じるのかという話もあるのか。


## eglotのevent管理の仕組み

```lisp
(defun eglot-events-buffer (server)
  "Display events buffer for SERVER."
  (interactive (list (eglot--current-server-or-lose)))
  (display-buffer (jsonrpc-events-buffer server)))

(defun eglot-stderr-buffer (server)
  "Display stderr buffer for SERVER."
  (interactive (list (eglot--current-server-or-lose)))
  (display-buffer (jsonrpc-stderr-buffer server)))

(defun eglot-forget-pending-continuations (server)
  "Forget pending requests for SERVER."
  (interactive (list (eglot--current-server-or-lose)))
  (jsonrpc-forget-pending-continuations server))
```
