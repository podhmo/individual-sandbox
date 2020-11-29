def gen():
    yield 1
    try:
        yield 2
    except Exception as e:
        print("!!", e)
        yield 10
    yield 3


g = gen()
for i in g:
    if i == 2:
        try:
            raise Exception("oops")
        except Exception as e:
            print("!", g.throw(e))
    print(i)
