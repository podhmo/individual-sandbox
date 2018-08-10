import json
import cgi
from wsgiref.simple_server import make_server
from handofcats import as_command


def app(environ, start_response):
    status = '200 OK'
    headers = [('Content-type', 'application/json; charset=utf-8')]
    start_response(status, headers)
    wsgi_input = environ["wsgi.input"]
    form = cgi.FieldStorage(fp=wsgi_input, environ=environ, keep_blank_values=True)
    # data: Dict[string, List[string]]  重複したparameterがあった場合
    data = {k: "**{}**".format(form[k].value) for k in form}
    return [json.dumps(data).encode("utf-8")]


@as_command()
def main(port=4444):
    httpd = make_server('', port, app)
    httpd.serve_forever()
