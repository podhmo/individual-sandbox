import mimetypes

x = "foo.jpg"
print(mimetypes.guess_type(x, strict=False))
x = "foo.jpeg"
print(mimetypes.guess_type(x, strict=False))
