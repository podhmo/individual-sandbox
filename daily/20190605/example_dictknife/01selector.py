from selector import Selector
from dictknife.jsonknife import get_resolver
from dictknife import loading


def get_selector(filename: str, *, r=None) -> Selector:
    return Selector(get_resolver(filename), r=r)


def main():
    selector = get_selector("person.json")
    loading.dumpfile(selector.select("#/name"), format="json")
    print("")

    selector = get_selector("person.json", r={})
    r = selector.select("name")
    r = selector.select("age")
    loading.dumpfile(r, format="json")
    print("")

    selector = get_selector("person.json", r={})
    r = selector.select("name")
    r = selector.select("age")
    r = selector.select("father")
    loading.dumpfile(r, format="json")
    print("")


if __name__ == "__main__":
    main()
