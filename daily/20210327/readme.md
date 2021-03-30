## python colorful output

- https://github.com/willmcgugan/rich
- https://github.com/ines/wasabi

## word-diff

- diff-highlight
- colorful-diff

```py
def color_diff(diff):
    for line in diff:
        if line.startswith("+"):
            line = line.replace("XXX", "\x1b[7mXXX\x1b[27m")
            yield f"\x1b[32m{line}\x1b[39m"
        elif line.startswith("-"):
            line = line.replace("XXX", "\x1b[7mXXX\x1b[27m")
            yield f"\x1b[31m{line}\x1b[39m"
        elif line.startswith("^"):
            yield f"\x1b[34m{line}\x1b[39m"
        else:
            yield line
```

これで良い説。

```console
$ diff -u <> | sed 's/^-/\x1b[1;31m-/;s/^+/\x1b[1;32m+/;s/^@/\x1b[1;34m@/;s/$/\x1b[0m/' | diff-highlight
```

diff-highlightは

```console
$  port contents git | grep "diff-highlight$"
  /opt/local/bin/diff-highlight
  /opt/local/share/git/contrib/diff-highlight/diff-highlight
```

### misc

- https://github.com/google/diff-match-patch
- https://udomomo.hatenablog.com/entry/2019/12/01/181404
- https://stackoverflow.com/questions/1721738/using-diff-or-anything-else-to-get-character-level-diff-between-text-files
- https://unix.stackexchange.com/questions/196565/how-to-color-diff-output
- https://future-architect.github.io/articles/20200610/
 
