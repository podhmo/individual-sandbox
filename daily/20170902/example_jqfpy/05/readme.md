Using jq to parse and display multiple fields in a json serially - Stack Overflow
https://stackoverflow.com/questions/28164849/using-jq-to-parse-and-display-multiple-fields-in-a-json-serially

```
$ jqfpy --squash -r '["{u[first]} {u[last]}".format(u=user) for user in get("users")]' data.json
Stevie Wonder
Michael Jackson
```
