## python flycheckに直す

## python jedi emacs

```
M-x list-packages
company-jediをインストール
```

```lisp
(with-eval-after-load 'python-mode
  (defun my:python-jedi-setup ()
    (add-to-list 'company-backends 'company-jedi))

  (add-hook 'python-mode-hook 'my:python-jedi-setup)
  )
```

このあたりが根幹。

```

(defun jedi:start-server ()
  (if (jedi:epc--live-p jedi:epc)
      (message "Jedi server is already started!")
    (setq jedi:epc (jedi:server-pool--start
                    (append jedi:server-command jedi:server-args))))
  jedi:epc)
```
## python epc

```console
$ pip install epc sexpdata
$ python -m epc.server --log-trace-back --allow-dotted-names os
```

## python jedi

```console
$ pip install jedi
$ PYTHONSTARTUP="$(python -m jedi repl)" python
```

?? jediを有効に

```
>>> from jedi.utils import setup_readline
>>> setup_readline()
```

@:何か違うのだよなー。


```python
>>> import jedi
>>> source = '''
... import datetime
... datetime.da'''
>>> script = jedi.Script(source, 3, len('datetime.da'), 'example.py')
>>> script
<Script: 'example.py'>
>>> completions = script.completions()
>>> completions                                         #doctest: +ELLIPSIS
[<Completion: date>, <Completion: datetime>, ...]
```
