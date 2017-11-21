def transform(d):
    d["fullName"] = "{d[firstName]} {d[lastName]}".format(d=d)
    return d
