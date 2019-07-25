import importlib.resources as r

with r.open_text("foo.data", "sample.txt") as rf:
    print(rf.read())
