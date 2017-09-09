import os.path

print("raw")
print("\t", os.path.exists("~/go/src/github.com/rogpeppe/godef/README"))

print("expanduser")
print("\t", os.path.exists(os.path.expanduser("~/go/src/github.com/rogpeppe/godef/README")))

print("expandvars")
print("\t", os.path.exists(os.path.expandvars("${GOPATH}/src/github.com/rogpeppe/godef/README")))
