# -*- coding:utf-8 -*-
import jwt

data = {"some": "payload"}
print("raw:", data)
token = jwt.encode(data, 'secret', algorithm="HS256")
print("encoded:", token)
data2 = jwt.decode(token, "secret")
print("decoded:", data2)
