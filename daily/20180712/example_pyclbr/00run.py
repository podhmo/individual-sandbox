import pyclbr

for name, val in pyclbr.readmodule_ex("re").items():
    print(name, vars(val))
