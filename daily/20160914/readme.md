# shell 手抜きのreplace

```
function replace () { grep -lr $1 . --exclude=.git | xargs gsed -i "s@$1@$2@g"; }
function replace2 () { echo "grep -lr \"$1\" .  --exclude=.git | xargs gsed -i \"s@$1@$2@g\";"; }
```
