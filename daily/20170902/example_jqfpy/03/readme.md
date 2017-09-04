How to filter an array of objects based on values in an inner array with jq? - Stack Overflow
https://stackoverflow.com/questions/26701538/how-to-filter-an-array-of-objects-based-on-values-in-an-inner-array-with-jq

```
$ make
jqfpy -r --squash 'import re; rx = re.compile("data"); [x["Id"] for x in get() if not any(rx.search(name) for name in x["Names"])]' data.json
cb94e7a42732b598ad18a8f27454a886c1aa8bbba6167646d8f064cd86191e2b
a4b7e6f5752d8dcb906a5901f7ab82e403b9dff4eaaeebea767a04bac4aada19
```

