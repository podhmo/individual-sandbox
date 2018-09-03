from prestring.python import Module
from prestring.utils import LParams

m = Module()

with m.class_("A0"):
    params0 = LParams()
    with m.method("f0", params0):
        m.stmt("pass")

with m.class_("A1"):
    params1 = LParams()
    with m.method("f1", params1):
        m.stmt("pass")

params1.append_tail("*args")
print(m)

# class A0:
#     def f0(self): <- this (not f0(self,))
#         pass



# class A1:
#     def f1(self, *args):
#         pass

