## python yamlのlinter

- [example_linter/](./example_linter/)

## python kamidana CIコケてた

これなんでだろう。

- https://travis-ci.org/podhmo/kamidana/jobs/571597913




## python dictknife CIコケてた

- https://github.com/podhmo/dictknife/issues/161

## python 3.8のbuild

```console
$ ./configure && make
...
Could not find platform independent libraries <prefix>
Could not find platform dependent libraries <exec_prefix>
Consider setting $PYTHONHOME to <prefix>[:<exec_prefix>]
Fatal Python error: init_fs_encoding: failed to get the Python codec of the filesystem encoding
ModuleNotFoundError: No module named 'encodings'
```

hmm

- https://bugs.python.org/issue35596
- https://stackoverflow.com/questions/19292957/how-can-i-troubleshoot-python-could-not-find-platform-independent-libraries-pr
- https://github.com/golemfactory/golem/issues/1401

### docker使ったほうが楽そう

- [example_py3.8/](./example_py3.8/)

