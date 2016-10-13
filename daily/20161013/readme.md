# git 過去の履歴も含めてgit grep

```
git grep $(git rev-list --branches=<branchname> -- <filename>)
```

本当は以下の様なことをしたい。

> 数あるコミットの中から、特定のファイルの特定の箇所が存在しないコミットを探す。
> (ただし特定のファイルは自動生成され、特定の箇所のフォーマットや位置が変わる)

参考

- [How to grep (search) committed code in the git history? - Stack Overflow](http://stackoverflow.com/questions/2928584/how-to-grep-search-committed-code-in-the-git-history)


## 頑張る

特定のブランチの特定のファイルの変更があったコミットを抜き出す

```
git rev-list --branches <target-branch> -- <target file>
```

変更があったコミットの内、パターンに一致する記述が存在するコミットを抜き出す。(sort -uによって時系列が壊れてしまう)

```
# git grep -i <pattern> $(<ここにrevlistを渡す>) | cut -d ':' -f 1 | sort -u
git grep -i <pattern> $(git rev-list --branches <target-branch> -- <target file>) | cut -d ':' -f 1 | sort -u
```

コミットを時系列順に並べ直す

```
# | git rev-list --stdin --date-order --no-walk につなぐ
git grep -i <pattern> $(git rev-list --branches <target-branch> -- <target file>) | cut -d ':' -f 1 | sort -u | git rev-list --stdin --date-order --no-walk
```

変更部分の周辺の部分を表示する

```
git grep -i <pattern> $(git rev-list --branches rook -- <target file>) | cut -d ':' -f 1 | sort -u | git rev-list --stdin --date-order --no-walk
git grep -i -A 5 <pattern> $(cat /tmp/revs) -- <target file>
```

# git 特定のrevisionのリストをコミット時刻で並べ替えたい

git logやgit showでちまちま確認したくない。

コミットのメッセージも確認したい

```
cat /tmp/revs | git log --oneline --date-order --no-walk --stdin
cat /tmp/revs | git log --pretty="format:%H,%cD,%cN,%s" --date-order --no-walk --stdin
```

並び替える

```
cat /tmp/revs | git rev-list --stdin  --date-order --no-walk
```

# python xml-rpc

簡単だしメモっておく
