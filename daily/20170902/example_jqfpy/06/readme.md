shell - How to merge 2 json file using jq? - Stack Overflow
https://stackoverflow.com/questions/19529688/how-to-merge-2-json-file-using-jq

```
$ jqfpy --slurp 'L = get(); from dictknife import deepmerge; d = deepmerge(*L); {"vaulue": d["value"]}'  00data.json 01data.json
{
  "vaulue": {
    "aaa": {
      "value1": "v1",
      "value2": "v2",
      "value3": "v3",
      "value4": 4
    },
    "bbb": {
      "value1": "v1",
      "value2": "v2",
      "value3": "v3"
    },
    "ccc": {
      "value1": "v1",
      "value2": "v2"
    },
    "ddd": {
      "value3": "v3",
      "value4": 4
    }
  }
}
```
