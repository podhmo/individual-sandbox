# -*- coding:utf-8 -*-
import cProfile
import re
import time
import pstats

profile = cProfile.Profile()
profile.enable()
time.sleep(0.2)
re.compile("foo|bar")
profile.disable()

s = pstats.Stats(profile)
print(s.total_tt)  # 0.20307900000000004
print(s.total_calls)  # 196
s.dump_stats("re.compile.prof")
