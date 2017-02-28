# python toybox すごい雑なpaginatorを試す用のserverを立てたい

```python
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
```

```bash
$ http :4444/value offset==20 limit==10
{
    "count": 10,
    "limit": 10,
    "offset": 20,
    "value": [
        21,
        22,
        23,
        24,
        25,
        26,
        27,
        28,
        29,
        30
    ]
}
```
