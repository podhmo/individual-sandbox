# git git-amで取り込みたい場合がある

```
git format-patch HEAD~..HEAD
git am 0001*.patch
```

# git git-stash

stashの中に残しておきたい場合にはapplyする

```
git stash
git stash pop
git stash apply stash@{1}
```
