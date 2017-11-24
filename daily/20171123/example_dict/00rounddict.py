# https://stackoverflow.com/questions/32434112/round-off-floating-point-values-in-dict


# My dictionary is:

d = [
    {
        'A': 0.700000000,
        'B': 0.255555555
    }, {
        'B': 0.55555555,
        'C': 0.55555555
    }, {
        'A': 0.255555555,
        'B': 0.210000000,
        'C': 0.2400000000
    }
]

# I need:

expected = [
    {
        'A': 0.70,
        'B': 0.25
    },
    {
        'B': 0.55,
        'C': 0.55
    },
    {
        'A': 0.25,
        'B': 0.21,
        'C': 0.24
    },
]

# hmm

from dictknife import dictmap  # noqa
got = dictmap(lambda x: round(x, 2) if isinstance(x, float) else x, d)
print(got)
# [{'A': 0.7, 'B': 0.26}, {'C': 0.56, 'B': 0.56}, {'C': 0.24, 'A': 0.26, 'B': 0.21}]
