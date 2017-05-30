from zenmai.naming import snakecase


def names(d):
    return "\n".join([snakecase(v["name"]) for v in d["X"].values()])
