from redbaron import RedBaron

# # monekey patching for prevent ValueError
# import redbaron.base_nodes
# def index(self, v):
#     try:
#         return self.data.index(v)
#     except ValueError:
#         return None
# redbaron.base_nodes.NodeList.index = index


source = """\
# comment1

def f(x, y):
    return x + y
"""


t2 = RedBaron(source)
t2.insert(3, "print('hello')")
print(t2.dumps())
