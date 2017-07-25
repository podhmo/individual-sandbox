# -*- coding:utf-8 -*-


class A:
    def __init__(self, value):
        self.value = value

    def __repr__(self):
        return self.vaue


def f(a):
    raise Exception(a)


if __name__ == "__main__":
    try:
        f(A(u"あ"))
    except Exception as e:
        # a.valule is not bytes. so, e.args[0] hasn't decode method'
        print(u"error is occured" + e.args[0].decode("utf-8"))
