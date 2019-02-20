from dictknife.accessing import Scope

s = Scope({"produces": ["/application/json"], "consumes": ["/application/json"]})
print(s[["produces"]])
with s.scope({"produces": ["/text/html"]}):
    print(s[["produces"]])
print(s[["produces"]])
