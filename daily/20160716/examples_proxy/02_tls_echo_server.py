# https://twistedmatrix.com/documents/current/core/howto/ssl.html#tls-echo-server
import sys

from twisted.internet import ssl, protocol, task, defer
from twisted.python import log
from twisted.python.modules import getModule
from twisted.internet.protocol import Protocol


class Echo(Protocol):
    def dataReceived(self, data):
        self.transport.write(data)


def main(reactor):
    log.startLogging(sys.stdout)
    certData = getModule("twisted.test").filePath.sibling('server.pem').getContent()
    print(certData)
    certificate = ssl.PrivateCertificate.loadPEM(certData)
    factory = protocol.Factory.forProtocol(Echo)
    reactor.listenSSL(8000, factory, certificate.options())
    return defer.Deferred()


if __name__ == '__main__':
    task.react(main)
