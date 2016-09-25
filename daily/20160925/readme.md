# git upstream branchに設定されていないものにcheckoutしようとして失敗

```
$ git checkout -b <branch> origin/<branch>
fatal: Cannot update paths and switch to branch '<branch>' at the same time.
Did you intend to checkout 'origin/<branch>' which can not be resolved as commit?
$ git remote set-branches --add origin <branch>
$ git fetch
$ git branch -avv
$ git checkout -b <branch> origin/<branch>
```

参考

- [git - "Cannot update paths and switch to branch at the same time" - Stack Overflow](http://stackoverflow.com/questions/22984262/cannot-update-paths-and-switch-to-branch-at-the-same-time)
