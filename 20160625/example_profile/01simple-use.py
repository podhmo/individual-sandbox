# -*- coding:utf-8 -*-
import cProfile
import re  # NOQA
cProfile.run('re.compile("foo|bar")', 're.compile.prof')

"""
 how to use `python -m pstats re.compile.prof`

% help
% stats
# とりあえず cumtimeを見つつpercallを見るという感じ？
% sort totime # total spent time in the given function
% sort percall # totime / ncalls
% sort cumtime # 全てのsub functionを含めた累積時間
"""
