### elm

```console
$ npm install -g elm
```

- elm-repl — play with Elm expressions
- elm-reactor — get a project going quickly
- elm-make — compile Elm code directly
- elm-package — download packages

repl

```console
$ elm repl
# : error while loading shared libraries: libtinfo.so.5
$ ldconfig -p | grep libtinfo
$ sudo ldconfig -v | grep libtinfo
ldconfig: Path `/usr/lib64' given more than once
ldconfig: Can't stat /usr/libx32: No such file or directory
        libncursesw.so.6 -> libtinfo.so.6

# work-around
# $ sudo ln -s /usr/lib/libncursesw.so.6 /usr/lib/libtinfo.so.5
$ sudo ln -s /usr/lib/libtinfo.so.6 /usr/lib/libtinfo.so.5

$ elm repl
---- elm-repl 0.18.0 -----------------------------------------------------------
 :help for help, :exit to exit, more at <https://github.com/elm-lang/elm-repl>
--------------------------------------------------------------------------------
> 1 / 2
0.5 : Float
> List.length [1,2,3,4]
4 : Int
```

## jupyter

jupyter > jupyter-client > jupyter-core

## python f-string getitem

```
python -c 'd = {"x": "y"}; print(f"""hello: {d["x"]}""")'
```


## github 対象のuserのrepositoryの一覧を取得

```console
$ TARGETNAME=jupyter
$ http https://api.github.com/users/${TARGETNAME}/repos sort==updated direct==desc
```

- https://developer.github.com/v3/repos/#list-user-repositories

```console
$ TARGETNAME=jupyter
$ http -b --pretty=format https://api.github.com/users/${TARGETNAME}/repos sort==updated direct==desc per_page==100 | jqfpy '[f"""git clone --depth=1 {d["ssh_url"]} """ for d in get()]' --squash -r | bash
```
