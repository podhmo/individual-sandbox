## python

対応しているversionの制限

- https://packaging.python.org/guides/dropping-older-python-versions/?highlight=release

pre-release package

- https://packaging.python.org/guides/distributing-packages-using-setuptools/?highlight=release#pre-release-versioning

## python 何かpipeなどで遊んでみたくなった

どうやるんだっけ？

- redirect
- pipe

### pipe

fork()してexec()的な感じだった記憶。dup2とか使う

### wait

os.wait*の種類多くない？

- os.wait()
- os.waitid()
- os.wait3()
- os.wait4()

- https://docs.python.org/3/library/os.html

このあたりだいぶ忘れているな。

### exec

execve, execvepとかって何が違うのだっけ？

- os.execl
- os.execle
- os.execlp
- os.execlpe
- os.execv
- os.execve
- os.execvp
- os.execvpe


### shell

- fork()
- pipe()
- execvp()
- dup2()

`dup2(src,dst,inherit=True)`

## python 複数行のprogressbarどうやるんだろう？

- https://github.com/peterbrittain/asciimatics
- https://zariganitosh.hatenablog.jp/entry/20150224/escape_sequence

```console
$ printf "\e7|-\e[-98b|"; for i in {0..100}; do printf "\e8|\e[${i}b"; sleep 0.05; done; echo
```


## terminal capture

### terminalizer

https://github.com/faressoft/terminalizer


```console
$ npm install -g terminalizer
$ terminalizer init
$ terminalizer record <cmd>

# render a recording file as animated gif image.
$ terminalizer render <file>
```

なんかダメっっぽいな

```
Please enter some details about your recording
? Title x
? Description x
? Tags such as git,bash,game linux

$HOME/.npm-global/lib/node_modules/terminalizer/commands/share.js:194
        return reject(body.errors.join('\n'));
                                  ^

TypeError: Cannot read property 'join' of undefined
    at Request._callback ($HOME/.npm-global/lib/node_modules/terminalizer/commands/share.js:194:35)
    at Request.self.callback ($HOME/.npm-global/lib/node_modules/terminalizer/node_modules/request/request.js:185:22)
    at Request.emit (events.js:197:13)
    at Request.<anonymous> ($HOME/.npm-global/lib/node_modules/terminalizer/node_modules/request/request.js:1161:10)
    at Request.emit (events.js:197:13)
    at IncomingMessage.<anonymous> ($HOME/.npm-global/lib/node_modules/terminalizer/node_modules/request/request.js:1083:12)
    at Object.onceWrapper (events.js:285:13)
    at IncomingMessage.emit (events.js:202:15)
    at endReadableNT (_stream_readable.js:1132:12)
    at processTicksAndRejections (internal/process/next_tick.js:76:17)
```

### asciinema

https://asciinema.org/


```conosole
$ yay -S community/asciinema
$ asciinema auth
$ asciinema rec xxx
$ asciinema upload xxx
```

- asciinema
- asciicast2gif

これだと絵文字が入っていないのでダメかも

```
$ docker run --rm -v $PWD:/data asciinema/asciicast2gif demo.cast output.gif
```

- https://github.com/asciinema/asciicast2gif

```
$ yay -S gifslice
$ npm install -g asciicast2gif
```

