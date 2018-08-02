s = "\u3010\u5317\u68ee\u66f4\u65b0\u9332"
print(s.encode("utf-8").decode())
print(s.encode("unicode-escape").decode())

b = s.encode("unicode-escape")
with open("x", "wb") as wf:
    wf.write(b)
with open("y", "w", encoding="utf-8") as wf:
    wf.write(s)
