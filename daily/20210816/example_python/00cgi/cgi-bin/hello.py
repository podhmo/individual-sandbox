#!/usr/local/bin/python3
import cgitb

cgitb.enable(display=0, logdir="logdir")
# http :8080/cgi-bin/hello.py
print("Content-Type: text/html; charset=utf-8\n")
print("こんにちは　日本語！")
