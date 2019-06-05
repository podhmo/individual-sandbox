from selector import Selector, transform_dict_with_respect_index
from dictknife.jsonknife import get_resolver
from dictknife import loading


def get_selector(filename: str, *, r=None) -> Selector:
    return Selector(get_resolver(filename), wrap=False, r=r)


def main():
    selector = get_selector("people.json")
    loading.dumpfile(selector.select(""), format="json")
    print("")
    selector = get_selector("people.json")
    loading.dumpfile(selector.select("0/name"), format="json")
    print("")
    selector = get_selector("people.json")
    loading.dumpfile(
        transform_dict_with_respect_index(selector.select("0/name")), format="json"
    )
    print("")
    selector = get_selector("people.json", r={})
    r = selector.select("1/name")
    loading.dumpfile(transform_dict_with_respect_index(r), format="json")
    print("")
    selector = get_selector("people.json", r={})
    r = selector.select("0/name@firstname")
    loading.dumpfile(transform_dict_with_respect_index(r), format="json")
    print("")


if __name__ == "__main__":
    main()
