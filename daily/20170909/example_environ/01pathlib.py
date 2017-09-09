import pathlib

print(pathlib.Path("~/go/src/github.com/rogpeppe/godef/README").exists())
print(pathlib.Path("~/go/src/github.com/rogpeppe/godef/README").expanduser().exists())
print(pathlib.Path("~/go/src/github.com/rogpeppe/godef/README").expandvars().exists())
