import string


def parse_template(d, formatter, r):
    if hasattr(d, "items"):
        for k, v in d.items():
            parse_template(k, formatter, r=r)
            parse_template(v, formatter, r=r)
    elif isinstance(d, (list, tuple)):
        for x in d:
            parse_template(x, formatter, r=r)
    else:
        for parsed in formatter.parse(d):
            if parsed[1] is not None:
                r.append(parsed[1])


def fill_template(d, kwargs, dict=dict):
    if hasattr(d, "items"):
        for k, v in d.items():
            return dict((fill_template(k, kwargs, dict=dict), fill_template(v, kwargs, dict=dict)) for k, v in d.items())
    elif isinstance(d, (list, tuple)):
        return [fill_template(x, kwargs, dict=dict) for x in d]
    elif isinstance(d, (str, bytes)):
        return d.format(**kwargs)
    else:
        return d


def mini_template(d, formatter=string.Formatter(), r=None):
    r = r or []
    parse_template(d, formatter, r)
    return list(set(r))


d = {
    "person": {
        "name{i}": "{name}",
        "age": "{age}"
    }
}

print(mini_template(d))
print(fill_template(d, {"i": 0, "name": "foo", "age": 20}))
