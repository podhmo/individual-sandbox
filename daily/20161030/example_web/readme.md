# server

```
make server
```

# client

```
$ make default
make api_register
http --json POST http://localhost:8080/auth/register \
	uid=1 \
	name=foo \
	password=password
{"token": "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJ1aWQiOiIxIn0.3Je8dg765BAtLPq-VB6AxAXKfusoAwiUsFruT8W2ymA"}make api_register2
http --json POST http://localhost:8080/auth/register \
	uid=2 \
	name=bar \
	password=password
{"token": "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJ1aWQiOiIyIn0.a35Y0zoqhbVVIOjtJBZ-y3JOfIz-N_npPDQyl1advnQ"}make api_token
http --json POST http://localhost:8080/auth/token \
	name=foo \
	password=password | tee dist/session
{"token": "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJ1aWQiOiIxIn0.3Je8dg765BAtLPq-VB6AxAXKfusoAwiUsFruT8W2ymA"}make api_me
http --json GET http://localhost:8080/me \
	"Authorization: Bearer `cat dist/session | jq .token -r`"
{"name": "foo", "uid": "1"}
```
