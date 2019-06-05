from selector import Selector
from dictknife.jsonknife import get_resolver
from dictknife import loading


def get_selector(filename: str, *, r=None) -> Selector:
    return Selector(get_resolver(filename), r=r)


def main():
    selector = get_selector("people.json")
    loading.dumpfile(selector.select(""), format="json")
    print("")


if __name__ == "__main__":
    main()
