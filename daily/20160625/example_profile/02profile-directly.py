# -*- coding:utf-8 -*-
import cProfile
import re
import time

profile = cProfile.Profile()
profile.enable()
time.sleep(0.2)
re.compile("foo|bar")
profile.disable()
profile.dump_stats("re.compile.prof")

import pstats
s = pstats.Stats("re.compile.prof")
# s.sort_stats("cumtime").print_stats()  # cumtime

# 初期化されるわけじゃないのでやっぱり使いまわせない。
profile.enable()
time.sleep(0.3)
re.compile("boo|bar")
profile.disable()

profile.dump_stats("re.compile2.prof")
s = pstats.Stats("re.compile2.prof")
# s.sort_stats("cumtime").print_stats()  # cumtime
print(s.files)
print(s.total_tt)
# for k, v in sorted(s.sort_arg_dict_default.items(), key=lambda xs: xs[1][0][0][0]):
#     print(k, v, v[-1])
"""
pcalls 0 primitive call count
ncalls 1 call count
calls 1 call count
tottime 2 internal time
time 2 internal time
cumulative 3 cumulative time
cumtime 3 cumulative time
module 4 file name
filename 4 file name
file 4 file name
line 5 line number
nfl 6 name/file/line
name 6 function name
stdname 7 standard name
"""
