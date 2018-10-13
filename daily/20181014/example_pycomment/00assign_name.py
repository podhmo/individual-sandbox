from lib2to3.fixer_util import Name, Assign

print(repr(Name("fooo")))
print(str(Assign(Name("foo"), Name("bar"))))
