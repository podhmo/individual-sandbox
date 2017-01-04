def internal(fn):
    fn(1)
    fn(2)
    fn(3)


def external():
    yield 1
    yield 2
    yield 3


# external -> internal
def as_internal(external, fn):
    for v in external():
        fn(v)


# internal -> external
def as_external(internal):
    # this is bad..
    xs = []

    def f(value):
        xs.append(value)
    internal(f)
    return xs


print("external")
for v in external():
    print(v)

print("internal")
internal(print)

print("external -> internal")
as_internal(external, print)

print("internal -> external")
for v in as_external(internal):
    print(v)


print("nested internal")


def nested(f):
    def g(v):
        def h(v2):
            f(v, v2)
        internal(h)  # xxx
    return g
internal(nested(print))

print("nested external")
for v in external():
    for v2 in external():
        print(v, v2)


print("nested external -> internal")  # hmm?
print("nested internal -> external")  # hmm??
