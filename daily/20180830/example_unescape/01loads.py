from handofcats import as_command
import json


@as_command
def run(path="./data.txt"):
    with open(path) as rf:
        s = rf.read()
    unescaped = s.encode("utf-8").decode("unicode-escape").strip('"\n ')
    print(path)
    print(s)
    print(unescaped)
    print(json.loads(unescaped))
