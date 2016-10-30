URL = http://localhost:8080

hello:
	echo hello

server:
	python app.py

default:
	make api_register
	make api_register2
	make api_token
	make api_me

api_register:
	http --json POST ${URL}/auth/register \
	uid=1 \
	name=foo \
	password=password

api_register2:
	http --json POST ${URL}/auth/register \
	uid=2 \
	name=bar \
	password=password


api_token:
	http --json POST ${URL}/auth/token \
	name=foo \
	password=password | tee dist/session

api_me:
	http --json GET ${URL}/me \
	"Authorization: Bearer `cat dist/session | jq .token -r`"
