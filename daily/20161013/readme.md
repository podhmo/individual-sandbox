# git 過去の履歴も含めてgit grep

```
git grep $(git rev-list --branches=<branchname> -- <filename>)
```

本当は以下の様なことをしたい。

数あるコミットの中から、特定のファイルの特定の箇所が存在しないコミットを探す。
(ただし特定のファイルは自動生成され、特定の箇所のフォーマットや位置が変わる)


参考

- [How to grep (search) committed code in the git history? - Stack Overflow](http://stackoverflow.com/questions/2928584/how-to-grep-search-committed-code-in-the-git-history)

# python xml-rpc

簡単だしメモっておく
