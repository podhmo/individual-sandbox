import base64

with open("./images.png", "rb") as rf:
    data = rf.read()
print(base64.b64encode(data))
