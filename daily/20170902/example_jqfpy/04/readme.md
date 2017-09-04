json - Select objects based on value of variable in object using jq - Stack Overflow
https://stackoverflow.com/questions/18592173/select-objects-based-on-value-of-variable-in-object-using-jq

```
jqfpy --squash '[(k, v["name"]) for k, v in get().items() if v["location"] == "Stockholm"]' data.json
[
  "FOO",
  "Donald"
]
[
  "BAR",
  "Walt"
]
```
