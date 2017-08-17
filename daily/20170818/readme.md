## unix command

findにはorとandもあった。.go以外と_test.goを列挙みたいなこともできる。

```
find . -not -name "*.go" -a  -name "*_test.go" -type f
```
