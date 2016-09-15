# golang text/template

- もしかしてprivateな変数にアクセス出来ない？ -> できなかった。
- 1.6でもダメだった

# ゆうちょ

- [記号番号から振込用の店名・預金種目・口座番号を調べる－ゆうちょ銀行](http://www.jp-bank.japanpost.jp/kojin/sokin/furikomi/kouza/kj_sk_fm_kz_1.html)
- [pdf ](http://www.jp-bank.japanpost.jp/kojin/sokin/furikomi/pdf/tenbangou_tenmei.pdf)

# git commitの名前などミスっていた

```
git filter-branch --commit-filter 'GIT_AUTHOR_NAME="podhmo"; GIT_AUTHOR_EMAIL="ababjam61+github@gmail.com"; GIT_COMMITTER_EMAIL="ababjam61+github@gmail.com" git commit-tree "$@"'
```

- https://git-scm.com/docs/git-filter-branch
