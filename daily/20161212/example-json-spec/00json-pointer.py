# -*- coding:utf-8 -*-
# json pointer

from jsonspec.pointer import extract

obj = {
    'foo': ['bar', 'baz', 'quux']
}
assert 'baz' == extract(obj, '/foo/1')
print(extract(obj, '/foo/1'))
