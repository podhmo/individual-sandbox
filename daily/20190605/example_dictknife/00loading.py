from dictknife import loading


loading.loadfile("string.json")  # => ['h', 'e', 'l', 'l', 'o']

with open("string.json") as rf:
    loading.load(rf)  # => 'hello'
