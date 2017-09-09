json - Complex jq Filter - Stack Overflow
https://stackoverflow.com/questions/24875419/complex-jq-filter


```
jqfpy --squash -c 'L = get("students"); [{"name": s["name"], "age": s["age"], "gpa": s["cum_gpa"], "CSC101": get("grades/CSC101", x)} for s in L for x in s["semesters"] if get("grades/CSC101", x) == "A"]' data.json
{"name": "John", "age": "19", "gpa": "3.83", "CSC101": "A"}
{"name": "Mary", "age": "20", "gpa": "3.50", "CSC101": "A"}
```
