# marshmallow importのobjectをそのままreprとして使いたい

hmm

# typescript の language server protocolで何かしたい

- [Microsoft/language-server-protocol: Defines a common protocol for language servers.](https://github.com/Microsoft/language-server-protocol)

# なんか色々と困ったやつ

- [about-home-keeping.md](https://gist.github.com/podhmo/96914fc37753013e885a91216202d5b5)
- [my-package-manager.md](https://gist.github.com/podhmo/37fe265460e3b6ef9b97e9886de3f356)

## 色々困っていたやつの対応

.bashrcに
```bash
alias rfind="python -c 'import os.path; import sys; cwd = os.getcwd().split(os.sep); suffix = sys.argv[1]; [print(path) for path in [os.sep.join([*cwd[:i], suffix]) for i in reversed(range(1, len(cwd) + 1))] if os.path.exists(path)]'"
```

を追加して

```bash
$ eval "`rfind bin/foo` x y z"
```

とかやる。
