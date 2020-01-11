import sys
import sys as x
import foo.bar
import foo.bar.baz
import foo.bar.baz as b
from i import j  # type: ignore
from i.j import k
from i.j import k as K
from ...xxx import zzz
from yyy.zzz import (
    AAA,
    BBB as bbb,
    CCC,
    DDD,
)

# sys -- Symbol(fullname='sys', name='sys')
# x -- Symbol(fullname='sys', name='x')
# foo.bar -- Symbol(fullname='foo.bar', name='foo.bar')
# foo.bar.baz -- Symbol(fullname='foo.bar.baz', name='foo.bar.baz')
# b -- Symbol(fullname='foo.bar.baz', name='b')
# j -- Symbol(fullname='i.j', name='j')
# k -- Symbol(fullname='i.j.k', name='k')
# K -- Symbol(fullname='k', name='K')
# zzz -- Symbol(fullname='...xxx.zzz', name='zzz')
# AAA -- Symbol(fullname='yyy.zzz.AAA', name='AAA')
# bbb -- Symbol(fullname='yyy.zzz.BBB', name='bbb')
# CCC -- Symbol(fullname='yyy.zzz.CCC', name='CCC')
# DDD -- Symbol(fullname='yyy.zzz.DDD', name='DDD')
