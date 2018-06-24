import logging
from epc.server import EPCServer

# from python-rpc's echo example


def add_server(address='localhost', port=0):
    server = EPCServer((address, port), log_traceback=True)

    ch = logging.StreamHandler()
    ch.setLevel(logging.DEBUG)
    server.logger.addHandler(ch)

    def add(*args):
        return sum(map(int, args))

    server.register_function(add)
    return server


if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG)
    server = add_server()
    server.print_port()  # needed for Emacs client

    server.serve_forever()
    server.logger.info('exit')
