from shape import parse, emit


class A:
    def a():
        pass


class B(A):
    def b():
        pass

    @classmethod
    def x():
        pass


print(emit(parse(B)))
