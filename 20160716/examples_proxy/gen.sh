#!/bin/sh

openssl genrsa 2048 > server.key
openssl req -new -key server.key > server.csr
openssl x509 -days 3650 -req -signkey server.key < server.csr > server.crt
# pem
openssl rsa -in server.key -text > server.pem
openssl x509 -inform PEM -in server.crt > client.pem
