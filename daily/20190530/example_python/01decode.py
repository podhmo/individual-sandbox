import urllib.parse as p

s = "%253Bstate%253D%257B%2522x%253A%2522%253A%252010%252C%2520%2522y%2522%253A%252020%252C%2520%2522name%2522%253A%2520%2522hello%2522%257D"

print(p.unquote(s.replace("%25", "%")))
