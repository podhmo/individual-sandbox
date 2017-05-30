from dictknife import loading
from zenmai.naming import snakecase

loading.setup()


path = "./app.yaml"
d = loading.loadfile(path)
print("\n".join([snakecase(v["name"]) for v in d["X"].values()]))
