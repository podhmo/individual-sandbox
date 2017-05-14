from kamidana import as_filter, as_test
import re


@as_filter
def prefix(v):
    return titleize(snakecase(v).split("_", 1)[0])


@as_filter
def swagger_path(v):
    return "swagger/{}.yaml".format(snakecase(v))


@as_filter
def swagger_ref(v):
    name = snakecase(v).rsplit("_", 1)[0]
    return "#/definitions/{}Data".format(camelcase(name))


@as_filter
def model_path(v):
    xs = snakecase(v).split("_")
    tag = xs[-1]
    name = xs[0]
    return "${{GOPATH}}/src/github.com/podhmo/model/gen_{}_{}.go".format(tag, name)


@as_test
def x(v):
    return "x" == snakecase(v).rsplit("_", 1)[1].lower()


# このあたりのコードはどこかライブラリに持っておきたい
def snakecase(
    name, rx0=re.compile('(.)([A-Z][a-z]+)'), rx1=re.compile('([a-z0-9])([A-Z])'), separator="_"
):
    pattern = r'\1{}\2'.format(separator)
    return rx1.sub(pattern, rx0.sub(pattern, name)).lower()


def camelcase(name):
    return untitleize(pascalcase(name))


def pascalcase(name, rx=re.compile("[\-_ ]")):
    return "".join(titleize(x) for x in rx.split(name))


def titleize(name):
    if not name:
        return name
    name = str(name)
    return "{}{}".format(name[0].upper(), name[1:])


def untitleize(name):
    if not name:
        return name
    return "{}{}".format(name[0].lower(), name[1:])
