from zenmai.loader import load
from dictknife import pp


with open("config.yaml") as rf:
    pp(load(rf)["development"])
