def gen():
    print("hai")
    yield 1
    print("hoi")


it = gen()
print(next(it))
it.throw(Exception, Exception("hmm"))
