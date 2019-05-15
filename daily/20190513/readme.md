## jsonschema 疑問

- patternPropertiesが重複してmatchしたものはどう扱う？
- additionalPropertiesはpatternPropertiesも含めたexcluded?

## arch remove orphan packages

orphan packageを見つけて消す

```console
$ yay -Qdt
$ yay -Rs $(yay -Qqdt)
```

updateがあるか調べる

```console
$ yay -Pu
```

### hmm

https://www.ostechnix.com/yay-found-yet-another-reliable-aur-helper/

## emacs IMEの変換中の表示が出ないのが不便

## emacs tourなんてあったんだ

https://www.gnu.org/software/emacs/tour/index.html
