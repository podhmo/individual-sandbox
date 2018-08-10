import json
from werkzeug.wrappers import Request, Response
from werkzeug.serving import run_simple
from handofcats import as_command


@Request.application
def app(request):
    data = {
        "name": "foo",
        "age": "20",
    }
    return Response(json.dumps(data), content_type="/application/json")


@as_command
def main(port=4444):
    run_simple("", port, app)
