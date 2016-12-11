from jsonspec.reference import resolve

obj = {
    'foo': ['bar', 'baz', {
        '$ref': '#/sub'
    }],
    'sub': 'quux'
}

assert 'quux' == resolve(obj, '#/foo/2')
