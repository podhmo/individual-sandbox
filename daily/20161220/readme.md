# emacs ansi colorを消す

```lisp
(replace-regexp "\[[0-9]+m" "")
```

# git 最近のrebase手順

最近のgitのrebaseの仕方

- dev(master)
- dev-A (feature-branch)

以下のような状況のとき

```
dev -> x -> y -> z
\--> -> i -> j -> dev-A
```

以下の様な形に安全にrebaseしたい

```
dev -> x -> y -> z -> i -> j -> dev-A
```

```bash
git fetch origin
git checkout dev-A
git checkout -b tmp
git glog
git rebase --onto remotes/origin/dev `git merge-base remotes/origin/dev` tmp
git checkout dev-A
git reset --hard tmp
```

rebaseに失敗しても変更はtmpだけなので、最悪tmpを捨てれば良いし。安全。

ちなみに rebaseの部分は以下でも良さそう

```bash
git br # tmp のことを確認
git rebase remotes/origin/dev
```
