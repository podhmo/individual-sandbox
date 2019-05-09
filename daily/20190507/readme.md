## python color zfill

- https://www.mm2d.net/main/prog/c/console-02.html
- https://note.nkmk.me/python-zero-padding/

## go too

```
$ go get -v github.com/otiai10/too
$ too
> python ./example_subprocess/gen.py
> python ./example_subprocess/gen.py
> python ./example_subprocess/gen.py
>
$ too --cmd "python ./example_subprocess/gen.py" --cmd "python ./example_subprocess/gen.py" --cmd "python ./example_subprocess/gen.py" --cmd "python ./example_subprocess/gen.py"

$ too --cmd "python -u -c 'for i in range(20): print(i)'" --cmd "python -u -c 'for i in range(20): print(i)'"
```

### hmm

自分自身のpidが見えれば十分なのでは？

