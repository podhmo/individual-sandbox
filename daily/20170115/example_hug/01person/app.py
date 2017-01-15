from functools import wraps
import falcon
import hug
import json


_store = None


def sync(fn):
    @wraps(fn)
    def wrapped(*args, **kwargs):
        setup()
        r = fn(*args, **kwargs)
        teardown()
        return r
    return wrapped


def setup(name="./store.json"):
    print("hai")
    global _store
    if _store is None:
        with open(name) as rf:
            _store = json.load(rf)


def teardown(name="./store.json"):
    print("hoi")
    global _store
    with open(name, "w") as wf:
        json.dump(_store, wf)


def get_store():
    return _store


@hug.cli()
@hug.get("/people")
@sync
def list_people():
    return get_store().get("people", [])


@hug.cli()
@sync
def create_person(name: hug.types.text, age: hug.types.number):
    if "people" not in _store:
        _store["people"] = []
    _store["people"].append({"name": name, "age": age})


@hug.post("/people")
def create_person_web(body):
    # see: http://www.hug.rest/website/learn/directives
    # see: https://github.com/timothycrosley/hug/blob/develop/examples/post_body_example.py
    data = json.loads(body)
    return create_person(data["name"], data["age"])


@hug.exception(json.decoder.JSONDecodeError)
def handle_invalid_json(response):
    # https://github.com/timothycrosley/hug/issues/380
    response.status = falcon.HTTP_400
    return {'error': 'invalid json'}
