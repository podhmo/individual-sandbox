from recommonmark.parser import CommonMarkParser
# from recommonmark.transform import AutoStructify

parser = CommonMarkParser()
with open("./src/doc.md") as rf:
    t = parser.parse(rf.read())
    print(t)
