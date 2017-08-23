from functools import partial


def strict_equal(pat):
    return lambda x: x == pat


def not_(matcher):
    return lambda x: not matcher(x)


def not_matcher(m):
    return lambda x: not_(m(x))


print(strict_equal("foo")("foo"))
print(strict_equal("foo")("bar"))
print(not_(strict_equal("foo"))("bar"))
print(not_matcher(strict_equal)("foo")("bar"))
