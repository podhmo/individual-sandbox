from toybox.simpleapi import simple_view, run


@simple_view("/hello/{name}")
def hello(request):
    return {"message": "hello {}".format(request.matchdict["name"])}


@simple_view("/500")
def error(request):
    return 10 / 0


@simple_view("/now")
def now(request):
    from datetime import datetime
    return {"now": datetime.now()}


def modify(config):
    from pyramid.renderers import JSON
    from datetime import datetime

    # override: json renderer
    json_renderer = JSON()

    def datetime_adapter(obj, request):
        return obj.isoformat()
    json_renderer.add_adapter(datetime, datetime_adapter)
    config.add_renderer('json', json_renderer)


if __name__ == "__main__":
    run.add_modify(modify)
    run(port=8080)
    # run.proutes()
