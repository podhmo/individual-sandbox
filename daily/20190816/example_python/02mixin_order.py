class A:
    def f(self):
        return "f"

    def h(self):
        return "hA"


class B:
    def g(self):
        return "g"

    def h(self):
        return "hB"


class AB(A, B):
    pass


class BA(B, A):
    pass


ab = AB()
(ab.__class__, ab.f(), ab.g(), ab.h())  # => (<class 'exec.AB'>, 'f', 'g', 'hA')
ba = BA()
(ba.__class__, ba.f(), ba.g(), ba.h())  # => (<class 'exec.BA'>, 'f', 'g', 'hB')

# ちなみにsocketserverの定義はこう
# if hasattr(os, "fork"):
#     class ForkingUDPServer(ForkingMixIn, UDPServer): pass
#     class ForkingTCPServer(ForkingMixIn, TCPServer): pass

# class ThreadingUDPServer(ThreadingMixIn, UDPServer): pass
# class ThreadingTCPServer(ThreadingMixIn, TCPServer): pass
