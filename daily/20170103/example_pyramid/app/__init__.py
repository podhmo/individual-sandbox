import logging
import os.path
from pyramid.config import Configurator
from pyramid.renderers import JSON
import datetime


def make_app(settings):
    config = Configurator(settings=settings)
    config.include("app.routes")

    # override: json renderer
    json_renderer = JSON()

    def datetime_adapter(obj, request):
        return obj.isoformat()
    json_renderer.add_adapter(datetime.datetime, datetime_adapter)
    config.add_renderer('json', json_renderer)

    return config.make_wsgi_app()


def main():
    from wsgiref.simple_server import make_server
    here = os.path.dirname(os.path.abspath(__file__))
    settings = {
        "here": here,
        "pyramid.reload_all": True,
    }
    app = make_app(settings)
    server = make_server('0.0.0.0', 8080, app)
    logging.basicConfig(level=logging.DEBUG)  # xxx
    server.serve_forever()


if __name__ == "__main__":
    main()
