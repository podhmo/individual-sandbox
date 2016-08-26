import re

s = "(?:\\.|[^\\])(;->>|;->)"
rx = re.compile("\\((?!\\?[:=!#])[^\\(\\)]+\\)")
m = rx.search(s)
print(s, m)
