class A:
    def m(self):
        return "m"


def call(fn):
    print("fn", fn)
    print("owner of fn", getattr(fn, "__self__", None))  # py2's im_self
    print("qualname", fn.__qualname__)
    return fn()


def f():
    return "f"


call(A().m)
print("----------------------------------------")
call(f)
# -- stdout --------------------
# >> fn <bound method A.m of <exec.A object at 0x7fe0b40242e8>>
# >> owner of fn <exec.A object at 0x7fe0b40242e8>
# >> qualname A.m
# >> ----------------------------------------
# >> fn <function f at 0x7fe0b40236a8>
# >> owner of fn None
# >> qualname f
