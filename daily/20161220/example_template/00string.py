s = "@%(foo)s@"
print(s % {"foo": "bar"})

# error
s = "@%(foo.bar)s@"
# print(s % {"foo": {"bar": "boo"}})
