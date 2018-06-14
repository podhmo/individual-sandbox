f = print
def generate():
    for i in range(5):
        f((yield i))


def consume(itr):
    for i in itr:
        if i == 2:
            itr.send(-i)
        print("\t", i)


consume(generate())
