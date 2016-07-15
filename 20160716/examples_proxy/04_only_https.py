import sys

from twisted.internet import protocol
from twisted.internet import reactor, ssl
from twisted.python.modules import getModule


class ConsoleWriter():
    def write(self, data, type):
        if (data):
            lines = data.decode("utf-8").split("\n")
            prefix = "<" if type == "request" else ">"
            for line in lines:
                print("%s %s" % (prefix, line))
        else:
            print("No response from server\n")


class DebugHttpClientProtocol(protocol.Protocol):
    def __init__(self, serverTransport):
        self.serverTransport = serverTransport

    def sendMessage(self, data):
        print("@@C sendMessage ", data)
        self.transport.write(data)

    def dataReceived(self, data):
        print("@@C dataReceived ", data)
        self.data = data
        ConsoleWriter().write(self.data, "response")
        self.serverTransport.write(self.data)

    def connectionLost(self, reason):
        self.serverTransport.loseConnection()
        self.transport.loseConnection()


class DebugHttpServerProtocol(protocol.Protocol):
    def dataReceived(self, data):
        print("@@S dataReceived ", data)
        self.data = data
        ConsoleWriter().write(self.data, "request")
        client = protocol.ClientCreator(reactor, DebugHttpClientProtocol, self.transport)

        # 証明書とか自分で取ってこないとダメ？
        certData = getModule(__name__).filePath.sibling('public.pem').getContent()
        authority = ssl.Certificate.loadPEM(certData)
        print(authority)
        options = ssl.optionsForClientTLS(u'google.com', authority)
        d = client.connectSSL(self.factory.targetHost, self.factory.targetPort, options)
        d.addCallback(self.forwardToClient, client)

    def forwardToClient(self, client, data):
        client.sendMessage(self.data)


class DebugHttpServerFactory(protocol.ServerFactory):
    protocol = DebugHttpServerProtocol

    def __init__(self, targetHost, targetPort):
        self.targetHost = targetHost
        self.targetPort = targetPort


def main():
    from twisted.python import log
    log.startLogging(sys.stdout)
    sourcePort = 4433
    targetHost, targetPort = sys.argv[1].split(":", 1)
    certData = getModule("twisted.test").filePath.sibling('server.pem').getContent()
    certificate = ssl.PrivateCertificate.loadPEM(certData)
    reactor.listenSSL(sourcePort, DebugHttpServerFactory(targetHost, int(targetPort)), certificate.options())
    reactor.run()


if __name__ == "__main__":
    main()
