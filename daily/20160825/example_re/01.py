# -*- coding:utf-8 -*-

# using lookahead assertion

import re

s = "abc"
s = "hogehogeaaabbbcccfugafuga"

rx = re.compile("aaa(bbb)ccc")
rx2 = re.compile("aaa(bbb)(?=ccc)")

m = rx.search(s)
print(m)

m2 = rx2.search(s)
print(m2)
