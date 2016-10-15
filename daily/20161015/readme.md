# git gitのrepository内の巨大なファイルを探す

現在のrevisionでサイズの大きなもの順に10件表示

```
$ git ls-tree -l -t -r --full-name HEAD | sort -n -k 4 -r | head -n 10
```

参考

- [How to find the N largest files in a git repository? - Stack Overflow](http://stackoverflow.com/questions/9456550/how-to-find-the-n-largest-files-in-a-git-repository)
- [filesize - Git: show total file size difference between two commits? - Stack Overflow](
http://stackoverflow.com/questions/10845051/git-show-total-file-size-difference-between-two-commits/10847242#10847242)
