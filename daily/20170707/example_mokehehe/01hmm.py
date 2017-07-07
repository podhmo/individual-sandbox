from io import StringIO

o = StringIO()
o.write("foo")
print(o.getvalue())
o.seek(0)
o.write("boo")
print(o.getvalue())
