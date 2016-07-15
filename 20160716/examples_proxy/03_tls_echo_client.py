from twisted.internet import ssl, task, protocol, endpoints, defer
from twisted.python.modules import getModule
from twisted.protocols.basic import LineReceiver


class EchoClient(LineReceiver):
    end = b"Bye-bye!"

    def connectionMade(self):
        self.sendLine(b"Hello, world!")
        self.sendLine(b"What a fine day it is.")
        self.sendLine(self.end)

    def lineReceived(self, line):
        print("receive:", line)
        if line == self.end:
            self.transport.loseConnection()


@defer.inlineCallbacks
def main(reactor):
    factory = protocol.Factory.forProtocol(EchoClient)
    certData = getModule(__name__).filePath.sibling('public.pem').getContent()
    authority = ssl.Certificate.loadPEM(certData)
    options = ssl.optionsForClientTLS(u'example.com', authority)
    endpoint = endpoints.SSL4ClientEndpoint(reactor, 'localhost', 8000,
                                            options)
    echoClient = yield endpoint.connect(factory)

    done = defer.Deferred()
    echoClient.connectionLost = lambda reason: done.callback(None)
    yield done

if __name__ == '__main__':
    task.react(main)
