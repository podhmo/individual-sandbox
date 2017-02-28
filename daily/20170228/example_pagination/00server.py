from toybox.simpleapi import simple_view, run
from pyramid.httpexceptions import HTTPBadRequest

L = list(range(1, 101))
assert len(L) == 100


@simple_view("/value")
def value(request):
    try:
        offset = int(request.GET.get("offset", 0))
        limit = int(request.GET.get("limit", 10))
    except ValueError as e:
        raise HTTPBadRequest(str(e))
    value = L[offset:][:limit]
    return {"value": value, "offset": offset, "limit": limit, "count": len(value)}


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--port", default=4444, type=int)
    args = parser.parse_args()
    run(port=args.port)
