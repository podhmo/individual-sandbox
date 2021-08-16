#!/usr/local/bin/python3
import cgitb
import json

cgitb.enable(display=0, logdir="logdir")
# http :8080/cgi-bin/app.py
print("Content-Type: application/json; charset=utf-8\n")
print(json.dumps([{"author": "foo", "text": "hello"}]))
