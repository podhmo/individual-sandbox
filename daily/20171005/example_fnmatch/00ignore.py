from fnmatch import fnmatch, translate


def check(name, pattern):
    print("check: pattern={}, name={}".format(pattern, name))
    print(fnmatch(name, pattern))


name = "test_x.py"
check(name, "test*.py")
check(name, "test_it*.py")
check(name, "test_[!i][!t]*.py")

name = "test_xx.py"
check(name, "test_[!i][!t]*.py")
print(translate("test_!"))
