from tinydb import TinyDB, Query

# https://tinydb.readthedocs.io/en/latest/usage.html

db = TinyDB("score.json")
db.purge()


db.insert({"name": "math", "score": 100})
db.insert({"name": "english", "score": 90})
db.insert({"name": "japanese", "score": 80})

S = Query()
print(db.count(S.score >= 90))
print(db.search(S.name.matches(".*h$")))
print(db.search(S.name.search("h$")))

print(db.search(S.name.test(lambda v: v in ["math", "japanese"])))
print(db.search((S.name == "math") | (S.name == "japanese")))
print(db.search((S.name == "math") & (S.score >= 80)))

# for collection, using all,any
