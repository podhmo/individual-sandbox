""" only http"""

from twisted.web import proxy, http
from twisted.internet import reactor


class MyProxyRequest(proxy.ProxyRequest):
    def process(self):
        print("request uri={}, method={}".format(self.uri, self.method))
        super().process()


class MyProxy(proxy.Proxy):
    requestFactory = MyProxyRequest


class ProxyFactory(http.HTTPFactory):
    protocol = MyProxy


def main():
    # import sys
    # from twisted.python import log
    # log.startLogging(sys.stderr)
    factory = ProxyFactory()
    reactor.listenTCP(8080, factory)
    reactor.run()


if __name__ == "__main__":
    main()
