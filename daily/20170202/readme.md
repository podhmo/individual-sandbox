# git 定期的にfilter-branchが必要になる

```bash
$ git filter-branch -f --commit-filter 'GIT_COMMITTER_NAME="podhmo" GIT_AUTHOR_NAME="podhmo"; GIT_AUTHOR_EMAIL="ababjam61+github@gmail.com"; GIT_COMMITTER_EMAIL="ababjam61+github@gmail.com" git commit-tree "$@"'
```
