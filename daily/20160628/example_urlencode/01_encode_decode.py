# -*- coding:utf-8 -*-
import urllib.parse as p


s0 = "%82%A0%82%A2%82%A4%82%A6%82%A8"  # urlencode unknown
s1 = "%E3%81%82%E3%81%84%E3%81%86%E3%81%88%E3%81%8A"  # urlencode utf-8

print(p.unquote(p.quote("あいうえお")) == "あいうえお")
print(p.unquote(s0))
print(p.unquote(s1))
