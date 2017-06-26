import sys
import csv

rows = [
    {
        "event": "foo",
        "message": "foo\tbar"
    },
    {
        "event": "bar",
        "message": 'foo"b"ar'
    },
    {
        "event": "boo",
        "message": "boo's something"
    },
]

fieldnames = ["message", "event"]
writer = csv.DictWriter(sys.stdout, fieldnames, delimiter="\t", doublequote=True, quoting=csv.QUOTE_ALL, strict=True)
writer.writeheader()
writer.writerows(rows)
