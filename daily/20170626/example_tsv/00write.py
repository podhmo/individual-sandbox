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

writer = csv.writer(sys.stdout, delimiter="\t", doublequote=True, quoting=csv.QUOTE_ALL, strict=True)
writer.writerows([(row["message"], row["event"]) for row in rows])
