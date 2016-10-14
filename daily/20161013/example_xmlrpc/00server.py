# -*- coding:utf-8 -*-
from xmlrpc import server
import math


class Handler(server.SimpleXMLRPCRequestHandler):
    rpc_paths = ('/RPC2', )


def make_app(port):
    s = server.SimpleXMLRPCServer(("localhost", port), requestHandler=Handler)
    s.register_introspection_functions()

    s.register_function(pow)
    s.register_instance(math)
    return s


def run_app(app):
    return app.serve_forever()

if __name__ == "__main__":
    run_app(make_app(4444))
