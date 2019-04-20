from urllib.parse import urlparse
from collections import defaultdict
from dictknife import loading
from dictknife import Accessor
from dictknife.langhelpers import make_dict
from handofcats import as_command
import makeschema as schemalib


def classify(d):
    r = defaultdict(list)
    for e in d["log"]["entries"]:
        url = e["request"]["url"]
        parsed = urlparse(url)
        k = parsed.hostname
        if parsed.port is not None:
            k = f"{k}:{parsed.port}"
        r[k].append(e)
    return r


def aggregate(entries, *, shy: bool = False):
    # path,method -> list
    r = defaultdict(lambda: defaultdict(list))
    for entry in entries:
        parsed = urlparse(entry["request"]["url"])

        mimetype = entry["response"]["content"].get("mimeType", "")

        if shy and not mimetype.startswith("application/json"):
            continue

        if mimetype.startswith("image/"):  # not including svg? (e.g. image/svg+xml)
            continue
        elif mimetype.startswith("text/css"):
            continue
        elif mimetype.startswith("application/font-woff"):
            continue

        if (
            entry["response"]["status"] in (200, 304)
            and entry["request"]["method"].lower() == "get"
        ):
            if "javascript" in mimetype and not parsed.path.endswith(".json"):
                continue
            elif "json" in mimetype and parsed.path.endswith(".map"):
                # sourcemap
                continue
        r[parsed.path][entry["request"]["method"]].append(entry)
    return r


def is_specific_header(
    name,
    value,
    *,
    _excludes=set(
        [
            "accept",
            "accept-encoding",
            "accept-language",
            "connection",
            "content-length",
            "content-type",
            "cookie",
            "host",
            "origin",
            "referer",
            "user-agent",
        ]
    ),
):
    name = name.lower()
    if name in _excludes:
        return False
    elif name.startswith("if-"):
        return False
    elif name.startswith("x-"):
        return False
    elif name.startswith("_"):
        return False
    else:
        return True


def get_value(name, value, *, candidates=["token", "xsrf", "session"]):
    name = name.lower()
    if any(x in name for x in candidates):
        r = []
        for x in value:
            if x.islower():
                r.append("x")
            elif x.isupper():
                r.append("X")
            elif x.isdigit():
                r.append("9")
            else:
                r.append(x)
        return "".join(r)
    return value


def transform(
    d,
    *,
    default_content_type="application/json",
    is_specific_header=is_specific_header,
    get_value=get_value,
    with_response_type=True,
    with_request_type=True,
    with_cookies=True,
    include_all=False,
):
    r = make_dict()
    a = Accessor()
    for path, methods in d.items():
        for method, entries in methods.items():
            d = {"description": ""}

            # todo: merge other output
            seen_parameters = defaultdict(set)
            for e in entries:
                # request

                # params :: path,query,header,cookie
                parameters = []
                for param_type, k, enabled in [
                    ("query", "queryString", True),
                    ("header", "headers", True),
                    ("cookie", "cookies", with_cookies),
                ]:
                    if not enabled:
                        continue

                    seen = seen_parameters[k]
                    for h in e["request"][k]:
                        if h["name"] in seen:
                            continue
                        seen.add(h["name"])
                        if include_all or is_specific_header(h["name"], h["value"]):
                            parameters.append(
                                {
                                    "name": h["name"],
                                    "in": param_type,
                                    "example": get_value(
                                        h["name"], h["value"]
                                    ),  # masking?
                                }
                            )
                if parameters:
                    d["parameters"] = parameters

                if e["request"].get("postData"):
                    post_data = e["request"]["postData"]
                    content_type = post_data["mimeType"].split(";", 1)[0]
                    if content_type.endswith("/json") and with_request_type:
                        d["requestBody"] = schemalib.makeschema(
                            loading.loads(post_data["text"], format="json")
                        )

                # response
                status = e["response"]["status"]

                content_type = e["response"]["content"].get("mimeType")
                if content_type is None:
                    for h in e["response"]["headers"]:
                        if h["name"].lower() == "content-type":
                            content_type = h["value"]
                            break
                    else:
                        content_type = default_content_type

                # "application/json; charset=utf-8" -> "application/json"
                content_type = content_type.split(";", 1)[0]

                schema = {}
                if content_type.startswith("text/"):
                    a.assign(schema, ["type"], "string")
                elif content_type.endswith("/json") and with_response_type:
                    response_body = loading.loads(
                        e["response"]["content"]["text"], format="json"
                    )
                    schema = schemalib.makeschema(response_body)

                a.assign(
                    d,
                    ["responses", status],
                    {
                        "description": e["response"]["statusText"],
                        "content": {content_type: schema},
                    },
                )
            a.assign(r, ["paths", path, method.lower()], d)
    return r


@as_command
def run(
    *,
    file: str,
    pattern: str = None,
    ignore: str = None,
    format: str = "json",
    shy: bool = False,
    with_request_type: bool = False,
    with_response_type: bool = False,
    ignore_cookies: bool = False,
    include_all: bool = False,
) -> None:
    import re

    pattern_rx = pattern and re.compile(pattern)
    ignore_rx = ignore and re.compile(ignore)

    d = loading.loadfile(file, format=format)
    for domain, entries in classify(d).items():
        if pattern_rx and pattern_rx.search(domain) is None:
            continue
        if ignore_rx and ignore_rx.search(domain) is not None:
            continue

        print("##", domain)
        print("")
        print("```yaml")
        r = aggregate(entries, shy=shy)
        loading.dumpfile(
            transform(
                r,
                with_request_type=with_request_type,
                with_response_type=with_response_type,
                with_cookies=not ignore_cookies,
                include_all=include_all,
            )
        )
        print("```")
        print("")
