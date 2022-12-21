import dis

# https://gihyo.jp/article/2022/07/monthly-python-2207


def run(beer_style: str) -> str:
    match beer_style:  # Pilsner, IPA, Hazy IPA and others
        case "Pilsner":
            result = "First drink"
        case "IPA":
            result = "I like it"
        case "Hazy IPA":
            result = "Cloudy and cloudy"
        case _:  # Wildcard
            result = "I like most beers"
    return result


def run2(beer_style: str) -> str:
    if beer_style == "Pilsner":
        result = "First drink"
    elif beer_style == "IPA":
        result = "I like it"
    elif beer_style == "Hazy IPA":
        result = "Cloudy and cloudy"
    else:  # Wildcard
        result = "I like most beers"
    return result


print(run("IPA"))
print(run("?"))
with open("00.run0.output", "w") as wf:
    dis.dis(run, file=wf)
with open("00.run1.output", "w") as wf:
    dis.dis(run, file=wf)
